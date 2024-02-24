htan_create_wide_tag_table <- function(){

  syn <- create_synapse_login()
  synapseclient <- reticulate::import("synapseclient")

  #1. LOADING DATA

  vanderbilt_files <- c("biospecimen"="syn38868462",
                        "demographics" ="syn38868669",
                        "diagnosis" = "syn39051142",
                        "followup" = "syn39051632",
                        "therapy" = "syn39051860"
  )

  files_path <- paste("inst/", vanderbilt_files, ".csv", sep = "") #locally stored, ideally will change to read directly from synapse
  vanderbilt_df <-  purrr::map(files_path, read.csv)
  names(vanderbilt_df) <- names(vanderbilt_files)

  #2. ADJUSTING TREATMENT INFORMATION

  #This dataset has NO data for treatment, so we will manually add the treatment info in iAtlas format

  therapy_iatlas <- vanderbilt_df$biospecimen %>%
    dplyr::select(
      "HTAN.Biospecimen.ID"
    ) %>%
    dplyr::mutate(
      "Sample_Treatment" = "pre_sample_treatment",
      "ICI_Rx" = "none_ICI_Rx",
      "Non_ICI_Rx" = "na_non_ici_rx",
      "Prior_ICI_Rx" =  "na_prior_ici_rx",
      "Prior_Rx" = "none_prior_rx",
      "Subsq_ICI_Rx" = "na_subsq_ici_rx",
      "Subsq_Rx" = "na_subsq_rx",
      "ICI_Pathway" = "none_ici_pathway",
      "ICI_Target" = "none_ICI_Target"
    )


  #3. CONSOLIDATING

  htan_tags <- vanderbilt_df[["biospecimen"]]%>%
    dplyr::select(
      "HTAN.Biospecimen.ID",
      "HTAN.Parent.ID",
      "Preservation.Method",
      "Collection.Days.from.Index",
      "Tumor.Tissue.Type",
      "Site.of.Resection.or.Biopsy",
      "Polyp.Histology"
    ) %>%
    dplyr::inner_join(
      dplyr::select(vanderbilt_df[["diagnosis"]], -Site.of.Resection.or.Biopsy), #removing duplicated information from biospecimen df
      by = dplyr::join_by("HTAN.Parent.ID" == "HTAN.Participant.ID")
    ) %>%
    dplyr::inner_join(
      therapy_iatlas,
      by = "HTAN.Biospecimen.ID"
    ) %>%
    dplyr::mutate(
      "Biopsy_Site_info" = dplyr::case_when(
        .data$Site.of.Resection.or.Biopsy == "Not Reported" ~ "Not available",
        TRUE ~ format_entry(.data$Site.of.Resection.or.Biopsy, c("Upper lobe ", "Intrathoracic ", " NOS", "s of head face and neck", "Lower lobe ", "Middle lobe ", "s of axilla or arm"), add_underscore = FALSE)),
      "Biopsy_Site" = dplyr::case_when(
        .data$Biopsy_Site_info == "Not available" ~ "na_biopsy_site",
        TRUE ~ paste0(format_entry(Biopsy_Site_info, "_", add_underscore = TRUE), "_biopsy_site")),
      "Cancer_Tissue_info" = dplyr::case_when(
        .data$Tissue.or.Organ.of.Origin == "Not Reported" ~ "Not available",
        TRUE ~ format_entry(.data$Tissue.or.Organ.of.Origin, (" NOS"))),
      "Cancer_Tissue" = dplyr::case_when(
        .data$Tissue.or.Organ.of.Origin == "Not Reported" ~ "na_cancer_tissue",
        TRUE ~ paste0(format_entry(.data$Tissue.or.Organ.of.Origin, (" NOS"), add_underscore = TRUE), "_cancer_tissue")),
      "Response" = "na_response",
      "Response_info" = "Not available",
      "Responder" = dplyr::case_when(
        Progression.or.Recurrence == "Yes - Progression or Recurrence"  ~ "false_responder",
        Progression.or.Recurrence == "No"  ~ "true_responder",
        Progression.or.Recurrence == "unknown"  ~ "na_responder",
        Progression.or.Recurrence == "Not Reported"  ~ "na_responder",
        is.na(Progression.or.Recurrence) ~ "na_responder"
      ),
      "Progression" = dplyr::case_when(
        Progression.or.Recurrence == "Yes - Progression or Recurrence" ~ "true_progression",
        Progression.or.Recurrence == "No"  ~ "false_progression",
        Progression.or.Recurrence == "unknown"  ~ "na_progression",
        Progression.or.Recurrence == "Not Reported"  ~ "na_progression",
        is.na(Progression.or.Recurrence) ~ "na_progression"
      ),
      "Clinical_Benefit" = dplyr::case_when(
        Progression.or.Recurrence == "Yes - Progression or Recurrence"  ~ "false_clinical_benefit",
        Progression.or.Recurrence == "No"  ~ "true_clinical_benefit",
        Progression.or.Recurrence == "unknown"  ~ "na_clinical_benefit",
        Progression.or.Recurrence == "Not Reported"  ~ "na_clinical_benefit",
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
        .data$Tumor.Tissue.Type == "Recurrent" ~ "false_metastasized",
        .data$Tumor.Tissue.Type == "Local recurrence" ~ "false_metastasized",
        .data$Tumor.Tissue.Type %in% c("Premalignant","Normal", "Atypia - hyperplasia") ~ "false_metastasized",
        .data$Tumor.Tissue.Type == "Not Otherwise Specified" ~ "na_metastasized",
        .data$Tumor.Tissue.Type == "" ~ "na_metastasized",
      ),
      "Clinical_Stage" = dplyr::case_when(
        is.na(.data$`AJCC.Clinical.Stage`) ~ "na_clinical_stage",
        .data$`AJCC.Clinical.Stage` == "Not Reported" ~ "na_clinical_stage",
        TRUE ~ .data$`AJCC.Clinical.Stage`
      ),
      "TCGA_Study" = "na_tcga_study",
      "TCGA_Study_info" = "Not available",
      "TCGA_Subtype" = "na_tcga_subtype",
      "TCGA_Subtype_info" = "Not available",
      "NeoICI_Rx" = "none_neoici_rx",
      "Tissue_Subtype" = "na_tissue_subtype",
      "Polyp_Histology_info" = dplyr::case_when(
        .data$Polyp.Histology == "" ~ "Not Available",
        TRUE ~ .data$Polyp.Histology
      ),
      "Polyp_Histology" = dplyr::case_when(
        .data$Polyp.Histology == "" ~ "na_polyp_histology",
        .data$Polyp.Histology == "Unknown" ~ "unknown_polyp_histology",
        TRUE ~ paste0(tolower(gsub(" - ", "_", .data$Polyp.Histology)), "_polyp_histology"),
      ),
      "Tumor_tissue_type_info" = dplyr::case_when( #ATTENTION! This is the same column used to generate the Metastasized annotation, but for Vanderbilt this annotation is key to group samples
        .data$Tumor.Tissue.Type == "Not Otherwise Specified" ~ "Not Available",
        TRUE ~ .data$Tumor.Tissue.Type
      ),
      "Tumor_tissue_type" = dplyr::case_when( #ATTENTION! This is the same column used to generate the Metastasized annotation
        .data$Tumor.Tissue.Type == "Not Otherwise Specified" ~ "na_tumor_tissue_type",
        TRUE ~ paste0(tolower(gsub(" - ", "_", .data$Tumor.Tissue.Type)), "_tumor_tissue_type")
      )
    )

  columns_with_labels <-  colnames(dplyr::select(htan_tags, dplyr::ends_with("info")))
  columns_with_names <- gsub("_info", "", columns_with_labels)


  tags_info <- htan_tags %>%
    dplyr::select(dplyr::all_of(columns_with_labels),
                  dplyr::all_of(columns_with_names)) %>%
    dplyr::rename_with(.cols=ends_with('_info'), ~ gsub("_info", "-short_display", .x)) %>%
    dplyr::rename_with(.cols=!ends_with('-short_display'), ~ paste0(.x, "-name")) %>%
    tidyr::pivot_longer(dplyr::everything(),
                        names_to = c("parent_tag", ".value"),
                        names_sep="-") %>%
    dplyr::filter(!is.na(short_display)) %>%
    dplyr::distinct()


  htan_tags <- htan_tags %>%
    dplyr::select(
      HTAN.Biospecimen.ID,
      HTAN.Parent.ID,
      Sample_Treatment,
      Biopsy_Site,
      Cancer_Tissue,
      Tissue_Subtype,
      Tumor_tissue_type,
      Polyp_Histology,
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

  readr::write_csv(htan_tags, "htan_vanderbilt_tags_wide.csv", na = "")
  file_entity <- synapseclient$File("htan_vanderbilt_tags_wide.csv", parent = "syn52570141")
  syn$store(file_entity)

  #writing key short labels
  readr::write_csv(tags_info, "htan_vanderbilt_labels.csv", na = "")
  file_entity <- synapseclient$File("htan_vanderbilt_labels.csv", parent = "syn52570141")
  syn$store(file_entity)

}

