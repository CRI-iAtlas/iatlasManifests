htan_build_tag_table <- function(){

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
        (Collection.Days.from.Index < Days.to.Treatment.Start) & !Therapeutic.Agents %in%  ici_agents ~ "Subsq",
        (Collection.Days.from.Index >= Days.to.Treatment.Start & Collection.Days.from.Index <= Days.to.Treatment.End) & Therapeutic.Agents %in%  ici_agents ~ "ICI",
        (Collection.Days.from.Index >= Days.to.Treatment.Start & Collection.Days.from.Index <= Days.to.Treatment.End) & !Therapeutic.Agents %in%  ici_agents ~ "Non_ICI",
        Collection.Days.from.Index > Days.to.Treatment.End & Therapeutic.Agents %in% ici_agents ~ "Prior_ICI",
        Collection.Days.from.Index > Days.to.Treatment.End & !Therapeutic.Agents %in% ici_agents ~ "Prior"
      )
    ) %>%
    dplyr::select(
      HTAN.Biospecimen.ID,
      Therapeutic.Agents,
      flag
    ) %>%
    dplyr::group_by(HTAN.Biospecimen.ID, flag) %>%
    dplyr::summarise(
      all_drugs = paste0(unique(Therapeutic.Agents), collapse = "_")
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
  therapy_iatlas$Non_ICI_Rx = paste0(dplyr::filter(therapy_iatlas, Sample_Treatment == "on_sample_treatment") %>%
                              dplyr::pull(Non_ICI) %>%
                              sort() %>%
                              tolower(),
                              "_non_ici_rx")
  therapy_iatlas$Prior_Rx = paste0(dplyr::filter(therapy_iatlas, Sample_Treatment == "on_sample_treatment") %>%
                                     dplyr::pull(Prior) %>%
                                     sort() %>%
                                     tolower(),
                                     "_prior_rx")
  therapy_iatlas$Subsq_Rx = paste0(dplyr::filter(therapy_iatlas, Sample_Treatment == "on_sample_treatment") %>%
                                     dplyr::pull(Subsq) %>%
                                     sort() %>%
                                     tolower(),
                                   "_subsq_rx")

  therapy_iatlas <- therapy_iatlas %>%
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
      "Biopsy_Site" = stringr::str_replace_all(
        gsub(" ", "_", tolower(paste0(.data$Site.of.Resection.or.Biopsy, "_biopsy_site"))),
        "_nos", ""
      ),
      "Cancer_Tissue" = stringr::str_replace_all(
        gsub(" ", "_", tolower(paste0(.data$Tissue.or.Organ.of.Origin, "_cancer_tissue"))),
        "_nos", ""
      ),
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
      "NeoICI_Rx" = "none_neoici_rx",
      "Tissue_Subtype" = "na_tissue_subtype",
      "Timepoint_Relative_Order" = dplyr::dense_rank(Collection.Days.from.Index)
   ) %>%
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

  }
