htan_create_wide_tag_table <- function(){

  syn <- create_synapse_login()
  synapseclient <- reticulate::import("synapseclient")

  #1. LOADING DATA

  ohsu_files <- c("biospecimen"="syn39141309",
                  "demographics" ="syn39141967",
                  "diagnosis" = "syn39141331",
                  "followup" = "syn39141359",
                  "therapy" = "syn39141376"
  )

  files_path <- paste("inst/", ohsu_files, ".csv", sep = "") #locally stored, ideally will change to read directly from synapse
  ohsu_df <-  purrr::map(files_path, read.csv)
  names(ohsu_df) <- names(ohsu_files)

  samples_ids <- c(
    "HTA9_1_1",
    "HTA9_1_9",
    "HTA9_1_33",
    "HTA9_1_86",
    "HTA9_1_97"
  )

  #2. ADJUSTING TREATMENT INFORMATION

  #listing the ICI drugs that were used in the treatment
  ici_agents <- c("Pembrolizumab", "Durvalumab")


  therapy_iatlas <- ohsu_df$biospecimen %>%
    dplyr::filter(HTAN.Biospecimen.ID %in% samples_ids) %>%
    dplyr::select(
      "HTAN.Biospecimen.ID",
      "HTAN.Parent.ID",
      "Collection.Days.from.Index"
    ) %>%
    dplyr::mutate(HTAN.Parent.ID = replace(HTAN.Parent.ID, HTAN.Parent.ID == "HTA9_1_6", "HTA9_1")) %>% #changing patient ID to version in Therapy
    dplyr::inner_join(dplyr::select(ohsu_df$therapy, HTAN.Participant.ID, Days.to.Treatment.Start, Days.to.Treatment.End, Therapeutic.Agents),
                      by = dplyr::join_by(HTAN.Parent.ID == HTAN.Participant.ID), relationship = "many-to-many") %>%
    dplyr::mutate(
      "flag" = dplyr::case_when(
        (Collection.Days.from.Index < Days.to.Treatment.Start) & Therapeutic.Agents %in%  ici_agents ~ "Subsq_ICI",
        (Collection.Days.from.Index < Days.to.Treatment.Start) & !Therapeutic.Agents %in%  ici_agents ~ "Subsq_info",
        (Collection.Days.from.Index >= Days.to.Treatment.Start & Collection.Days.from.Index <= Days.to.Treatment.End) & Therapeutic.Agents %in%  ici_agents ~ "ICI",
        (Collection.Days.from.Index >= Days.to.Treatment.Start & Collection.Days.from.Index <= Days.to.Treatment.End) & !Therapeutic.Agents %in%  ici_agents ~ "Non_ICI_info",
        Collection.Days.from.Index > Days.to.Treatment.End & Therapeutic.Agents %in% ici_agents ~ "Prior_ICI",
        Collection.Days.from.Index > Days.to.Treatment.End & !Therapeutic.Agents %in% ici_agents ~ "Prior_info"
      )
    ) %>%
    dplyr::select(
      HTAN.Biospecimen.ID,
      Therapeutic.Agents,
      flag
    ) %>%
    dplyr::group_by(HTAN.Biospecimen.ID, flag) %>%
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
            is.na(ICI) & is.na(Prior_ICI) ~ "pre_sample_treatment",
            ICI == "Pembrolizumab" ~ "on_sample_treatment",
            is.na(ICI) & Prior_ICI == "Pembrolizumab" ~ "post_sample_treatment"
          ),
      "ICI_Rx" = "pembro_ici_rx",
      "ICI_Pathway" = "pd1_ici_pathway",
      "ICI_Target" = "pd1_ici_target",
      "Prior_ICI_Rx" = "none_prior_ici_rx",
      "Subsq_ICI_Rx" = "none_subsq_ici_rx",
    )

  #The pre and post treatment information in iAtlas is recorded using the ICI treatment as reference, so in this dataset all samples will have the same info for these fields
  therapy_info <- therapy_iatlas %>%
    dplyr::filter(Sample_Treatment == "on_sample_treatment") %>%
    dplyr::select(Non_ICI_info, Prior_info, Subsq_info) %>%
    dplyr::mutate(
      "Non_ICI_Rx" = paste0(tolower(stringi::stri_replace_all_regex(.data$Non_ICI_info, c(", ", " "), "_", vectorize_all=FALSE)), "_non_ici_rx"),
      "Prior_Rx" = paste0(tolower(stringi::stri_replace_all_regex(.data$Prior_info, c(", ", " "), "_", vectorize_all=FALSE)), "_prior_rx"),
      "Subsq_Rx" = paste0(tolower(stringi::stri_replace_all_regex(.data$Subsq_info, c(", ", " "), "_", vectorize_all=FALSE)), "_subsq_rx"),
    )


  therapy_labels <- therapy_info %>%
    dplyr::rename_with(.cols=ends_with('_info'), ~ gsub("_info", "_Rx-short_display", .x)) %>%
    dplyr::rename_with(.cols=!ends_with('-short_display'), ~ paste0(.x, "-name")) %>%
    tidyr::pivot_longer(dplyr::everything(),
                        names_to = c("parent_tag", ".value"),
                        names_sep="-")

  therapy_iatlas <- therapy_iatlas %>%
    dplyr::mutate(
      "Non_ICI_Rx" = therapy_info$Non_ICI_Rx,
      "Prior_Rx" = therapy_info$Prior_Rx,
      "Subsq_Rx" = therapy_info$Subsq_Rx
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
  htan_tags <- ohsu_df[["biospecimen"]] %>%
    dplyr::filter(HTAN.Biospecimen.ID %in% samples_ids) %>%
    dplyr::select(
      "HTAN.Biospecimen.ID",
      "HTAN.Parent.ID",
      "Preservation.Method",
      "Collection.Days.from.Index",
      "Tumor.Tissue.Type",
      "Site.of.Resection.or.Biopsy"
    ) %>%
    dplyr::mutate(HTAN.Parent.ID = replace(HTAN.Parent.ID, HTAN.Parent.ID == "HTA9_1_6", "HTA9_1")) %>% #changing patient ID to version in Therapy
    dplyr::inner_join(
      dplyr::select(ohsu_df[["diagnosis"]], -Site.of.Resection.or.Biopsy), #removing duplicated information from biospecimen df
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
      "Response" = dplyr::if_else(
        HTAN.Biospecimen.ID == "HTA9_1_86",
        "progressive_disease_response", #we have this information from the manuscript, missing for other samples
        "na_response"
      ),
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
      "TCGA_Study" = "BRCA",
      "TCGA_Subtype" = dplyr::case_when( # From manuscript: Classification using the PAM50 subtype gene signature 15 showed liver biopsies Bx1, Bx2, and Bx4 to be luminal A, whereas the bone biopsy Bx3 was luminal B
        Biopsy_Site == "liver_biopsy_site" ~ "BRCA_LumA",
        Biopsy_Site == "bone_biopsy_site" ~ "BRCA_LumB",
        TRUE ~ "na_tcga_subtype"
      ),
      "TCGA_Subtype_info" = dplyr::if_else(
        TCGA_Subtype == "na_tcga_subtype",
        "Not available",
        NA_character_
      ),
      "NeoICI_Rx" = "none_neoici_rx",
      "Tissue_Subtype" = "na_tissue_subtype",
      "Timepoint_Relative_Order" = dplyr::dense_rank(Collection.Days.from.Index)
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
      Timepoint_Relative_Order,
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

  readr::write_csv(htan_tags, "htan_ohsu_tags_wide.csv", na = "")
  file_entity <- synapseclient$File("htan_ohsu_tags_wide.csv", parent = "syn52570141")
  syn$store(file_entity)

  #writing key short labels
  readr::write_csv(tags_info, "htan_ohsu_labels.csv", na = "")
  file_entity <- synapseclient$File("htan_ohsu_labels.csv", parent = "syn52570141")
  syn$store(file_entity)

  }
