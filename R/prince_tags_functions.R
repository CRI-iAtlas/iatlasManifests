prince_build_tag_table <- function(){

  ici_tags <- synapse_csv_id_to_tbl(syn, "syn51613683")

  demographics <- synapse_csv_id_to_tbl(syn, "syn51251905")
  samples <- synapse_csv_id_to_tbl(syn, "syn51252936")
  #consolidating previous and post treatments
  radiation_pre <- synapse_csv_id_to_tbl(syn, "syn51251999")
  radiation_after <- synapse_csv_id_to_tbl(syn, "syn51252021")
  surgery_pre <- synapse_csv_id_to_tbl(syn, "syn51252008")
  surgery_after  <- synapse_csv_id_to_tbl(syn, "syn51252025")
  drugs_pre <- synapse_csv_id_to_tbl(syn, "syn51252006")
  drugs_after <- synapse_csv_id_to_tbl(syn, "syn51252023")

  trial_drugs <- synapse_csv_id_to_tbl(syn, "syn51251938")

  # annotations from samples, subjects and demographics
  prince_tags <- samples %>%
    dplyr::inner_join(
      subjects, by = "subject.id"
    ) %>%
    dplyr::inner_join(
      timepoints, by = "timepoint.id"
    ) %>%
    dplyr::inner_join(
      demographics, by = dplyr::join_by(subject.id == "Subject ID")
    ) %>%
    dplyr::mutate(
      "Response" = dplyr::case_when(
        bor == "SD" ~ "stable_disease_response",
        bor == "PD" ~ "progressive_disease_response",
        bor == "PR" ~ "partial_response_response",
        bor == "CR" ~ "complete_response_response",
        is.na(bor) ~ "na_response"
      ),
      "Responder" = dplyr::case_when(
        bor %in% c("PR","CR" )  ~ "true_responder",
        bor %in% c("PD","SD" )  ~ "false_responder",
        is.na(bor) ~ "na_responder"
      ),
      "Progression" = dplyr::case_when(
        bor %in% c("PD")  ~ "true_progression",
        bor %in% c("PR","CR","SD")  ~ "false_progression",
        is.na(bor) ~ "na_progression"
      ),
      "Clinical_Benefit" = dplyr::case_when(
        bor %in% c("PD")  ~ "false_clinical_benefit",
        bor %in% c("PR","CR","SD")  ~ "true_clinical_benefit",
        is.na(bor) ~ "na_clinical_benefit"
      ),
      "Biopsy_Site" = stringr::str_replace_all(
        gsub(" ", "_", tolower(paste0(.data$gdc.anatomic.site.name, "_biopsy_site"))),
        "right_middle_lobe,", ""
      ),
      "FFPE" = dplyr::if_else(
        stringr::str_detect(.data$sample.specimen, "ffpe"),
        "true_ffpe",
        "false_ffpe",
        missing = "na_ffpe"
      ),
      "Sample_Treatment" = dplyr::case_when(
        timepoint.type == ":timepoint.type/baseline" ~ "pre_sample_treatment",
        timepoint.type == ":timepoint.type/on-treatment" ~ "on_sample_treatment",
        timepoint.type == ":timepoint.type/eot" ~ "post_sample_treatment"
      ),
      "Clinical_Stage" = dplyr::case_when(
        `Stage at Initial Diagnosis` == "Stage 1" ~ "I",
        `Stage at Initial Diagnosis` == "Stage 2" ~ "II",
        `Stage at Initial Diagnosis` == "Stage 3" ~ "III",
        `Stage at Initial Diagnosis` == "Stage 4" ~ "IV"
      ),
      "Tissue_Subtype" = gsub(" ", "_", tolower(paste0(.data$`Cancer Location`, "_tissue_subtype"))),
      "Cancer_Tissue" = "pancreas_cancer_tissue",
      "Metastasized" = "true_metastasized"
    ) %>%
    dplyr::select(
      "sample.id",
      "subject.id",
      Response,
      Responder,
      Clinical_Benefit,
      Sample_Treatment,
      Biopsy_Site,
      FFPE,
      Clinical_Stage,
      Tissue_Subtype,
      Cancer_Tissue,
      Metastasized
    )

  # annotations from trial treatment

  key_df <- dplyr::tibble("Arm" = character(),
                          "ICI_Rx" = character(),
                          "ICI_Pathway" = character(),
                          "ICI_Target" = character(),
                          "Non_ICI_Rx" = character()) %>%
    dplyr::add_row("Arm" = "A1",
                   "ICI_Rx" = "nivolumab",
                   "ICI_Pathway" = "pd1_ici_pathway",
                   "ICI_Target" = "pd1_ici_target",
                   "Non_ICI_Rx" = "nab-paclitaxel_gemcitabine_non_ici_rx") %>%
    dplyr::add_row("Arm" = "B1",
                   "ICI_Rx" = "none_ICI_Rx",
                   "ICI_Pathway" = "none_ici_pathway",
                   "ICI_Target" = "none_ICI_Target",
                   "Non_ICI_Rx" = "nab-paclitaxel_gemcitabine_APX005M_non_ici_rx")  %>%
    dplyr::add_row("Arm" = "B2",
                   "ICI_Rx" = "none_ICI_Rx", #CHECK
                   "ICI_Pathway" = "none_ICI_Rx", #CHECK
                   "ICI_Target" ="none_ICI_Rx", #CHECK
                   "Non_ICI_Rx" = "nab-paclitaxel_gemcitabine_APX005M_non_ici_rx")  %>%
    dplyr::add_row("Arm" = "C1",
                   "ICI_Rx" = "nivolumab",
                   "ICI_Pathway" = "pd1_ici_pathway",
                   "ICI_Target" = "pd1_ici_target",
                   "Non_ICI_Rx" = "nab-paclitaxel_gemcitabine_APX005M_non_ici_rx") %>%
    dplyr::add_row("Arm" = "C2",
                   "ICI_Rx" = "nivolumab",
                   "ICI_Pathway" = "pd1_ici_pathway",
                   "ICI_Target" = "pd1_ici_target",
                   "Non_ICI_Rx" = "nab-paclitaxel_gemcitabine_APX005M_non_ici_rx")




}
