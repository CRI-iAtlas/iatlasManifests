htan_create_wide_tag_table <- function(){

  syn <- create_synapse_login()
  synapseclient <- reticulate::import("synapseclient")

  #MSK
  #1. LOADING DATA

  msk_files <- c("biospecimen"="syn39256250",
                  "demographics" ="syn39253974",
                  "diagnosis" = "syn39254633",
                  "followup" = "syn39255125",
                  "therapy" = "syn39255232"
  )

  files_path <- paste("inst/", msk_files, ".csv", sep = "") #locally stored, ideally will change to read directly from synapse
  msk_df <-  purrr::map(files_path, read.csv)
  names(msk_df) <- names(msk_files)

  #2. ADJUSTING TREATMENT INFORMATION

  #listing the ICI drugs that were used in the treatment
  ici_agents <- c("pembrolizumab", "nivolumab", "ipilimumab", "durvalumab", "atezolizumab")

  ici_targets <- setNames(c("pd1", "pd1", "ctla4", "pdl1", "pdl1"), ici_agents)
  ici_pathways <- setNames(c("pd1", "pd1", "ctla4", "pd1", "pd1"), ici_agents)

  therapy_iatlas <- msk_df$biospecimen %>%
    dplyr::select(
      "HTAN.Biospecimen.ID",
      "HTAN.Parent.ID",
      "Collection.Days.from.Index"
    ) %>%
    dplyr::inner_join(dplyr::select(msk_df$therapy, HTAN.Participant.ID, Days.to.Treatment.Start, Days.to.Treatment.End, Therapeutic.Agents),
                      by = dplyr::join_by(HTAN.Parent.ID == HTAN.Participant.ID), relationship = "many-to-many") %>%
    dplyr::filter(!is.na(Days.to.Treatment.Start) & !is.na(Days.to.Treatment.End) & Therapeutic.Agents != "") %>%
    dplyr::mutate(
      Days.to.Treatment.End = ifelse(
        is.na(Days.to.Treatment.End),
        Days.to.Treatment.Start,
        Days.to.Treatment.End
      )
    ) %>%
    dplyr::mutate(
      "flag" = dplyr::case_when(
        (Collection.Days.from.Index < Days.to.Treatment.Start) & stringr::str_detect(Therapeutic.Agents, paste0(ici_agents, collapse = '|')) ~ "Subsq_ICI_info",
        (Collection.Days.from.Index < Days.to.Treatment.Start) & !stringr::str_detect(Therapeutic.Agents, paste0(ici_agents, collapse = '|'))~ "Subsq_info",
        (Collection.Days.from.Index >= Days.to.Treatment.Start & Collection.Days.from.Index <= Days.to.Treatment.End) & stringr::str_detect(Therapeutic.Agents, paste0(ici_agents, collapse = '|')) ~ "ICI",
        (Collection.Days.from.Index >= Days.to.Treatment.Start & Collection.Days.from.Index <= Days.to.Treatment.End) & !stringr::str_detect(Therapeutic.Agents, paste0(ici_agents, collapse = '|')) ~ "Non_ICI_info",
        Collection.Days.from.Index > Days.to.Treatment.End & stringr::str_detect(Therapeutic.Agents, paste0(ici_agents, collapse = '|')) ~ "Prior_ICI_info",
        Collection.Days.from.Index > Days.to.Treatment.End & !stringr::str_detect(Therapeutic.Agents, paste0(ici_agents, collapse = '|')) ~ "Prior_info"
      )
    ) %>%
    dplyr::select(
      HTAN.Biospecimen.ID,
      HTAN.Parent.ID,
      Therapeutic.Agents,
      flag
    ) %>%
    dplyr::group_by(HTAN.Parent.ID, HTAN.Biospecimen.ID, flag) %>%
    dplyr::mutate(
      Therapeutic.Agents = ifelse(nchar(Therapeutic.Agents) == 0, NA_character_, Therapeutic.Agents)
    ) %>%
    dplyr::summarise(
      all_drugs = paste0(sort(unique(Therapeutic.Agents)), collapse = ", "),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = flag,
      values_from = all_drugs
    ) %>%
    dplyr::mutate(
      "Sample_Treatment" = dplyr::case_when(
        !is.na(ICI)  ~ "on_sample_treatment",
        is.na(ICI) & is.na(Prior_ICI_info) ~ "pre_sample_treatment",
        is.na(ICI) & !is.na(Prior_ICI_info) ~ "post_sample_treatment"
      ),
      "Non_ICI_info" = dplyr::case_when(
        is.na(ICI) ~ Non_ICI_info,
        !is.na(ICI) & is.na(Non_ICI_info) ~ gsub("^\\++|\\+$", "", (stringr::str_remove_all(ICI, paste0(ici_agents, collapse = '|')))),
        !is.na(ICI) & !is.na(Non_ICI_info) ~ paste(Non_ICI_info, gsub("^\\++|\\+$", "",(stringr::str_remove_all(ICI, paste0(ici_agents, collapse = '|')))), sep = "_")
      )
    ) %>%
    dplyr::rowwise() %>%
      dplyr::mutate(
        "Non_ICI_info" = ifelse(nchar(Non_ICI_info) == 0, NA_character_, Non_ICI_info),
        "ICI" = dplyr::case_when(
          is.na(ICI) ~ ICI,
          !is.na(ICI) ~ paste0(sort(unique(unlist(stringr::str_extract_all(ICI, paste0(ici_agents, collapse = '|'))))), collapse = "_")
        )
    )

  #check if all samples from same patient have the same treatment information - so far we hadn't have this issue, but will need to accommodate that in case this happens
  patient_therapy <- dplyr::n_distinct(dplyr::select(therapy_iatlas, -HTAN.Biospecimen.ID))

  stopifnot(patient_therapy == dplyr::n_distinct(therapy_iatlas$HTAN.Parent.ID))
  ######


  therapy_info <- therapy_iatlas %>%
    dplyr::select(ICI_info = ICI, Non_ICI_info, Prior_ICI_info, Prior_info, Subsq_ICI_info, Subsq_info) %>%
    dplyr::mutate(
      "ICI_Rx" = tolower(stringi::stri_replace_all_regex(.data$ICI_info, c(", ", " ", "\\+"), "_", vectorize_all=FALSE)),
      "Non_ICI_Rx" = paste0(tolower(stringi::stri_replace_all_regex(.data$Non_ICI_info, c(", ", " ", "\\+"), "_", vectorize_all=FALSE)), "_non_ici_rx"),
      "Prior_ICI_Rx" = paste0(tolower(stringi::stri_replace_all_regex(.data$Prior_ICI_info, c(", ", " ", "\\+"), "_", vectorize_all=FALSE)), "_prior_ici_rx"),
      "Prior_Rx" = paste0(tolower(stringi::stri_replace_all_regex(.data$Prior_info, c(", ", " ", "\\+"), "_", vectorize_all=FALSE)), "_prior_rx"),
      "Subsq_ICI_Rx"= paste0(tolower(stringi::stri_replace_all_regex(.data$Subsq_ICI_info, c(", ", " ", "\\+"), "_", vectorize_all=FALSE)), "_subsq_ici_rx"),
      "Subsq_Rx" = paste0(tolower(stringi::stri_replace_all_regex(.data$Subsq_info, c(", ", " ", "\\+"), "_", vectorize_all=FALSE)), "_subsq_rx"),
    ) %>%
    dplyr::mutate( #deal with NAs
      "ICI_Rx" = ifelse(is.na(ICI_info), "na_ici_rx", ICI_Rx),
      "Non_ICI_Rx" = ifelse(is.na(Non_ICI_info), "na_non_ici_rx", Non_ICI_Rx),
      "Prior_ICI_Rx" = ifelse(is.na(Prior_ICI_info), "na_prior_ici_rx", Prior_ICI_Rx),
      "Prior_Rx" = ifelse(is.na(Prior_info), "na_prior_rx", Prior_Rx),
      "Subsq_ICI_Rx" = ifelse(is.na(Subsq_ICI_info), "na_subsq_ici_rx", Subsq_ICI_Rx),
      "Subsq_Rx" = ifelse(is.na(Subsq_info), "na_subsq_rx", Subsq_Rx)
    )


  therapy_labels <- therapy_info %>%
    dplyr::mutate( #deal with NAs
      "ICI_info" = ifelse(is.na(ICI_info), "Not available", ICI_info),
      "Non_ICI_info" = ifelse(is.na(Non_ICI_info), "Not available", Non_ICI_info),
      "Prior_ICI_info" = ifelse(is.na(Non_ICI_info), "Not available", Non_ICI_info),
      "Prior_info" = ifelse(is.na(Prior_info), "Not available", Prior_info),
      "Subsq_info" = ifelse(is.na(Subsq_info), "Not available", Subsq_info)
    ) %>%
    dplyr::rename_with(.cols=ends_with('_info'), ~ gsub("_info", "_Rx-short_display", .x)) %>%
    dplyr::rename_with(.cols=!ends_with('-short_display'), ~ paste0(.x, "-name")) %>%
    tidyr::pivot_longer(dplyr::everything(),
                        names_to = c("parent_tag", ".value"),
                        names_sep="-") %>%
    dplyr::distinct()

  therapy_iatlas <- therapy_iatlas %>%
    dplyr::bind_cols(
      dplyr::select(therapy_info, ICI_Rx, Prior_ICI_Rx, Non_ICI_Rx, Prior_Rx, Subsq_ICI_Rx, Subsq_Rx)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      "ICI_Pathway" = dplyr::case_when(
        ICI_Rx == "na_ici_rx" ~ "none_ici_pathway",
        ICI_Rx %in% ici_agents ~ paste0(ici_pathways[ICI_Rx], "_ici_pathway"),
        !is.na(ICI_Rx) & !ICI_Rx %in% ici_agents ~ paste(paste0(sort(unique(ici_pathways[stringr::str_split(ICI_Rx, "_", simplify = TRUE)])), collapse = "_"), "_ici_pathway")
      ),
      "ICI_Target" = dplyr::case_when(
        ICI_Rx == "na_ici_rx" ~ "none_ICI_Target",
        ICI_Rx %in% ici_agents ~ paste0(ici_targets[ICI_Rx], "_ici_target"),
        !is.na(ICI_Rx) & !ICI_Rx %in% ici_agents ~ paste(paste0(sort(unique(ici_targets[stringr::str_split(ICI_Rx, "_", simplify = TRUE)])), collapse = "_"), "_ici_target")
      )
    ) %>%
    dplyr::select(
      HTAN.Biospecimen.ID,
      Sample_Treatment,
      ICI_Rx,
      ICI_Pathway,
      ICI_Target,
      Prior_ICI_Rx,
      Subsq_ICI_Rx,
      Non_ICI_Rx,
      Prior_Rx,
      Subsq_Rx
    )


  #3. CONSOLIDATING
  htan_tags <- msk_df[["biospecimen"]] %>%
    #dplyr::filter(HTAN.Biospecimen.ID %in% samples_ids) %>%
    dplyr::select(
      "HTAN.Biospecimen.ID",
      "HTAN.Parent.ID",
      "Preservation.Method",
      "Collection.Days.from.Index",
      "Tumor.Tissue.Type",
      "Site.of.Resection.or.Biopsy"
    ) %>%
   # dplyr::mutate(HTAN.Parent.ID = replace(HTAN.Parent.ID, HTAN.Parent.ID == "HTA9_1_6", "HTA9_1")) %>% #changing patient ID to version in Therapy
    dplyr::inner_join(
      dplyr::select(msk_df[["diagnosis"]], -Site.of.Resection.or.Biopsy), #removing duplicated information from biospecimen df
      by = dplyr::join_by("HTAN.Parent.ID" == "HTAN.Participant.ID")
    ) %>%
    dplyr::inner_join(
      therapy_iatlas,
      by = "HTAN.Biospecimen.ID"
    ) %>%
    dplyr::mutate(
      "Biopsy_Site" = paste0(tolower(gsub(" NOS", "", .data$Site.of.Resection.or.Biopsy)), "_biopsy_site"),
      "Biopsy_Site_info" = gsub(" NOS", "", .data$Site.of.Resection.or.Biopsy),
      "Cancer_Tissue" = paste0(tolower(gsub(" NOS", "", .data$Tissue.or.Organ.of.Origin)), "_cancer_tissue"),
      "Cancer_Tissue_info" = gsub(" NOS", "", .data$Tissue.or.Organ.of.Origin),
      #"Response" = dplyr::if_else(
      #   HTAN.Biospecimen.ID == "HTA9_1_86",
      #   "progressive_disease_response", #we have this information from the manuscript, missing for other samples
      #   "na_response"
      # ),
      "Responder" = dplyr::case_when(
        Progression.or.Recurrence == "Yes - Progression or Recurrence"  ~ "false_responder",
        #bor %in% c("PD","SD" )  ~ "true_responder", #this dataset only has Yes values for this, update with value for No is encoded
        is.na(Progression.or.Recurrence) ~ "na_responder"
      ),
      "Progression" = dplyr::case_when(
        Progression.or.Recurrence == "Yes - Progression or Recurrence" ~ "true_progression",
        #bor %in% c("PR","CR","SD")  ~ "false_progression", #this dataset only has Yes values for this, update with value for No is encoded
        is.na(Progression.or.Recurrence) ~ "na_progression"
      ),
      "Clinical_Benefit" = dplyr::case_when(
        Progression.or.Recurrence == "Yes - Progression or Recurrence"  ~ "false_clinical_benefit",
        #bor %in% c("PR","CR","SD")  ~ "true_clinical_benefit",  #this dataset only has Yes values for this, update with value for No is encoded
        is.na(Progression.or.Recurrence) ~ "na_clinical_benefit"
      ),
      "FFPE" = dplyr::case_when(
        .data$Preservation.Method == "Formalin fixed paraffin embedded - FFPE" ~ "true_ffpe",
        .data$Preservation.Method %in% c("", "Not Reported") ~ "na_ffpe",
        TRUE ~ "false_ffpe"
      ),
      "Metastasized" = dplyr::case_when(
        .data$Tumor.Tissue.Type == "Metastatic" ~ "true_metastasized",
        .data$Tumor.Tissue.Type == "Primary" ~ "false_metastasized",
        .data$Tumor.Tissue.Type == "" ~ "na_metastasized",
      ),
      "Clinical_Stage" = dplyr::if_else(
        is.na(.data$`AJCC.Clinical.Stage`),
        "na_clinical_stage",
        .data$`AJCC.Clinical.Stage`
      ),
      # "TCGA_Study" = "BRCA",
      # "TCGA_Subtype" = dplyr::case_when( # From manuscript: Classification using the PAM50 subtype gene signature 15 showed liver biopsies Bx1, Bx2, and Bx4 to be luminal A, whereas the bone biopsy Bx3 was luminal B
      #   Biopsy_Site == "liver_biopsy_site" ~ "BRCA_LumA",
      #   Biopsy_Site == "bone_biopsy_site" ~ "BRCA_LumB",
      #   TRUE ~ "na_tcga_subtype"
      # ),
      # "TCGA_Subtype_info" = dplyr::if_else(
      #   TCGA_Subtype == "na_tcga_subtype",
      #   "Not available",
      #   NA_character_
      # ),
      "NeoICI_Rx" = "none_neoici_rx",
      # "Tissue_Subtype" = "na_tissue_subtype"
    )


  tags_info <- htan_tags %>%
    dplyr::select(dplyr::ends_with("info"),
                  c("Cancer_Tissue", "Biopsy_Site", "TCGA_Subtype")) %>%
    dplyr::rename_with(.cols=ends_with('_info'), ~ gsub("_info", "-short_display", .x)) %>%
    dplyr::rename_with(.cols=!ends_with('-short_display'), ~ paste0(.x, "-name")) %>%
    tidyr::pivot_longer(dplyr::everything(),
                        names_to = c("parent_tag", ".value"),
                        names_sep="-") %>%
    dplyr::filter(!is.na(short_display)) %>%
    dplyr::distinct() %>%
    rbind(therapy_labels)


  htan_tags <- htan_tags %>%
    dplyr::select(
      HTAN.Biospecimen.ID,
      HTAN.Parent.ID,
      Sample_Treatment,
      Biopsy_Site,
      Cancer_Tissue,
      Tissue_Subtype,
      Response,
      Responder,
      Progression,
      Clinical_Benefit,
      FFPE,
      Metastasized,
      Clinical_Stage,
      TCGA_Study,
      TCGA_Subtype,
      NeoICI_Rx,
      ICI_Rx,
      ICI_Pathway,
      ICI_Target,
      Prior_ICI_Rx,
      Subsq_ICI_Rx,
      Non_ICI_Rx,
      Prior_Rx,
      Subsq_Rx
    )

  readr::write_csv(htan_tags, "htan_msk_tags_wide.csv", na = "")
  file_entity <- synapseclient$File("htan_msk_tags_wide.csv", parent = "syn52570141")
  syn$store(file_entity)

  #writing key short labels
  readr::write_csv(tags_info, "htan_msk_labels.csv", na = "")
  file_entity <- synapseclient$File("htan_msk_labels.csv", parent = "syn52570141")
  syn$store(file_entity)

}
