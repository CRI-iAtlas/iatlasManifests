htan_build_tag_table <- function(){

  synapseclient <- reticulate::import("synapseclient")

  #1. LOADING DATA

  ohsu_files <- c("biospecimen"="syn39141309", #biospecimen
                  "demographics" ="syn39141967", #demographics
                  "diagnosis" = "syn39141331", #diagnosis
                  "followup" = "syn39141359" #follow up
  )

  files_path <- paste("inst/", ohsu_files, ".csv", sep = "") #locally stored, ideally will change to read directly from synapse
  ohsu_df <-  purrr::map(files_path, read.csv)
  names(ohsu_df) <- names(ohsu_files)


  #2.
  htan_tags <- ohsu_df[["biospecimen"]] %>%
    dplyr::select(
      "HTAN.Biospecimen.ID",
      "HTAN.Parent.ID",
      "Preservation.Method"
    ) %>%
    dplyr::inner_join(
      ohsu_df[["diagnosis"]],
      by = dplyr::join_by("HTAN.Parent.ID" == "HTAN.Participant.ID")
    ) %>%
    dplyr::mutate(
      "Biopsy_Site" = stringr::str_replace_all(
        gsub(" ", "_", tolower(paste0(.data$Site.of.Resection.or.Biopsy, "_biopsy_site"))),
        " NOS", ""
      ),
      "Biopsy_Site_info" = stringr::str_replace_all(
        .data$Site.of.Resection.or.Biopsy,
        " NOS", ""
      ),
      "Cancer_Tissue" = stringr::str_replace_all(
        gsub(" ", "_", tolower(paste0(.data$Site.of.Resection.or.Biopsy, "_cancer_tissue"))),
        " NOS", ""
      ),
      "Cancer_Tissue_info" = stringr::str_replace_all(
        .data$Site.of.Resection.or.Biopsy,
        " NOS", ""
      ),
      "Response" = dplyr::if_else(
        "HTAN.Biospecimen.ID" == "HTA9_1_86",
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
    #   "Sample_Treatment" = dplyr::case_when(
    #     timepoint.type == ":timepoint.type/baseline" ~ "pre_sample_treatment",
    #     timepoint.type == ":timepoint.type/on-treatment" ~ "on_sample_treatment",
    #     timepoint.type == ":timepoint.type/eot" ~ "post_sample_treatment"
    #   ),
    #   "Clinical_Stage" = dplyr::case_when(
    #     `Stage at Initial Diagnosis` == "Stage 1" ~ "i_clinical_stage",
    #     `Stage at Initial Diagnosis` == "Stage 2" ~ "ii_clinical_stage",
    #     `Stage at Initial Diagnosis` == "Stage 3" ~ "iii_clinical_stage",
    #     `Stage at Initial Diagnosis` == "Stage 4" ~ "iv_clinical_stage"
    #   ),
    #   "Clinical_Stage_info" = dplyr::case_when(
    #     `Stage at Initial Diagnosis` == "Stage 1" ~ "I",
    #     `Stage at Initial Diagnosis` == "Stage 2" ~ "II",
    #     `Stage at Initial Diagnosis` == "Stage 3" ~ "III",
    #     `Stage at Initial Diagnosis` == "Stage 4" ~ "IV"
    #   ),
    #   "ICI_Rx" = dplyr::if_else(
    #     .data$subject.id %in% b2_with_nivo$`Subject ID`,
    #     "nivolumab",
    #     .data$ICI_Rx
    #   ),
    #   "ICI_Pathway" = dplyr::if_else(
    #     .data$subject.id %in% b2_with_nivo$`Subject ID`,
    #     "pd1_ici_pathway",
    #     .data$ICI_Pathway
    #   ),
    #   "ICI_Target" = dplyr::if_else(
    #     .data$subject.id %in% b2_with_nivo$`Subject ID`,
    #     "pd1_ici_target",
    #     .data$ICI_Target
    #   ),
    #   "Prior_ICI_Rx" = "none_prior_ici_rx",
    #   "Subsq_ICI_Rx" = dplyr::if_else(
    #     subject.id %in% names(post_ici_treatment),
    #     paste0(post_ici_treatment[subject.id], "_subsq_ici_rx") ,
    #     "none_subsq_ici_rx"
    #   ),
    #   "Subsq_ICI_Rx_info" = dplyr::if_else(
    #     subject.id %in% names(post_ici_treatment),
    #     post_ici_treatment[subject.id],
    #     "none"
    #   ),
    #   "Prior_Rx" = dplyr::if_else(
    #     is.na(.data$Prior_Rx),
    #     "none_prior_rx",
    #     paste0(.data$Prior_Rx, "_prior_rx")
    #   ),
    #   "Subsq_Rx" = dplyr::if_else(
    #     is.na(.data$Subsq_Rx),
    #     "none_subsq_rx",
    #     paste0(.data$Subsq_Rx, "_subsq_rx")
    #   ),
    #   "Tissue_Subtype" = gsub(" ", "_", tolower(paste0(.data$`Cancer Location`, "_tissue_subtype"))),
    #   "Tissue_Subtype_info" =.data$`Cancer Location`,
    #   "TCGA_Study" = "PDAC",
    #   "TCGA_Study_info" = "PDAC",
    #   "NeoICI_Rx" = "none_neoici_rx",
    #   "Metastasized" = "true_metastasized"
   )
    #
    #
    #
    #  %>%
    # dplyr::select(
    #   "HTAN.Biospecimen.ID",
    #   "HTAN.Parent.ID",
    #   "Biopsy_Site"
    # )

  }
