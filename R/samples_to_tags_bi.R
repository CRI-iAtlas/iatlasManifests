samples_to_tags_bi <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()


  #getting clinical data from the patient table
  #Info in this table:
  #"Clinical_Stage", Biopsy site, "Response", "ICI_Pathway", "ICI_Target", "Non_ICI_Rx", "Metastasized"
  patient_info <-
    syn$get("syn60531525") %>%
    purrr::pluck("path") %>%
    openxlsx::read.xlsx(., sheet = 2) %>%
    dplyr::as_tibble()

  colnames(patient_info) <- patient_info[1,]
  patient_info <- patient_info[-1,]
  patient_info <- patient_info %>%
    dplyr::mutate("sample_name" = paste0("Bi_ccRCC_", Sample))


  #getting ids
  samples <-
    synapse_csv_id_to_tbl(syn, "syn60531805") %>%
    dplyr::select(
      "sample_name" = "name",
      "sample_id" = "id"
    )

  tags <-
    synapse_csv_id_to_tbl(syn, "syn53698018") %>% #msk tags
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn60548945") #bi tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn60157438") #li tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn58896103") #shiao tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn59210643") #krishna tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn53697423") #vanderbilt tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn51613683") #ici tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn51080176")  #add tags from TCGA
    ) %>%
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )

  #consolidating
  samples_to_tags <- samples %>%
    dplyr::inner_join(patient_info, by = "sample_name") %>%
    dplyr::mutate(
      "gender" = dplyr::if_else(
        Sex == "F",
        "female",
        "male"
      ),
      "Clinical_Stage" = dplyr::case_when(
        Stage == "IV" ~ "iv_clinical_stage",
        Stage == "I" ~ "i_clinical_stage"
      ),
      "Metastasized" = dplyr::case_when(
        Stage == "I" ~ "false_metastasized",
        Stage == "IV" ~ "true_metastasized"
      ),
      "TCGA_Subtype" = dplyr::if_else(
        Histology == "Clear cell",
        "RCC_ccRCC",
        "na_tcga_subtype"
      ),
      "Biopsy_Site" = dplyr::case_when(
        `Biopsy Site` %in% c("Lymph node  ", "Lymph node") ~ "lymph_node_biopsy_site",
        `Biopsy Site` == "Kidney" ~ "kidney_biopsy_site",
        `Biopsy Site` == "Lung" ~ "lung_biopsy_site",
        `Biopsy Site` == "Abdominal Mass " ~ "abdomen_biopsy_site"
      ),
      "Response" = dplyr::case_when(
        `ICB Response` %in% c("NE", "N/A") ~ "na_response",
        `ICB Response` == "PR" ~ "partial_response_response",
        `ICB Response` == "SD" ~ "stable_disease_response",
        `ICB Response` == "PD" ~ "progressive_disease_response"
      ),
      "Responder" = dplyr::case_when(
        `ICB Response` %in% c("NE", "N/A") ~ "na_responder",
        `ICB Response` == "PR" ~ "true_responder",
        `ICB Response` == "SD" ~ "false_responder",
        `ICB Response` == "PD" ~ "false_responder"
      ),
      "Clinical_Benefit" = dplyr::case_when(
        `ICB Response` %in% c("NE", "N/A") ~ "na_clinical_benefit",
        `ICB Response` == "PR" ~ "true_clinical_benefit",
        `ICB Response` == "SD" ~ "true_clinical_benefit",
        `ICB Response` == "PD" ~ "false_clinical_benefit"
      ),
      "Progression" = dplyr::case_when(
        `ICB Response` %in% c("NE", "N/A") ~ "na_progression",
        `ICB Response` == "PR" ~ "false_progression",
        `ICB Response` == "SD" ~ "false_progression",
        `ICB Response` == "PD" ~ "true_progression"
      ),
      "ICI_Pathway" = dplyr::case_when(
        ICB %in% c("aPD-1", "aPD1 + VEGF TKI") ~ "pd1_ici_pathway",
        ICB == "aPD-1 + aCTLA-4" ~ "ctla4_pd1_ici_pathway",
        ICB == "No ICB" ~ "none_ici_pathway"
      ),
      "ICI_Target" = dplyr::case_when(
        ICB %in% c("aPD-1", "aPD1 + VEGF TKI") ~ "pd1_ici_target",
        ICB == "aPD-1 + aCTLA-4" ~ "ctla4_pd1_ici_target",
        ICB == "No ICB" ~ "none_ICI_Target"
      ),
      "ICI_Rx" = dplyr::case_when(
        ICB %in% c("aPD-1", "aPD1 + VEGF TKI", "aPD-1 + aCTLA-4") ~ "unnamed_ICI_Rx",
        ICB == "No ICB" ~ "none_ICI_Rx"
      ),
      "Non_ICI_Rx" = dplyr::case_when(
        `TKI-exposed` %in% "TKI" ~ "none_non_ici_rx",
        `TKI-exposed` == "No TKI" ~ "vegf_tki_unnamed_non_ici_rx"
      ),
      "Sample_Treatment" = dplyr::case_when(
        ICB %in% c("aPD-1", "aPD1 + VEGF TKI", "aPD-1 + aCTLA-4") ~ "post_sample_treatment",
        ICB == "No ICB" ~ "na_sample_treatment"
      )
    ) %>%
    dplyr::select(
      sample_id,
      gender,
      Clinical_Stage,
      Metastasized,
      TCGA_Subtype,
      Biopsy_Site,
      Response,
      Responder,
      Clinical_Benefit,
      Progression,
      ICI_Pathway,
      ICI_Target,
      ICI_Rx,
      Non_ICI_Rx
    ) %>%
    dplyr::mutate( #these categories are defined by the study protocol
      "race" = "na_race",
      "ethnicity" = "na_ethnicity",
      "Tumor_tissue_type" = "na_tumor_tissue_type",
      "Tissue_subtype" = "na_tissue_subtype",
      "Polyp_Histology" = "na_polyp_histology",
      "Cancer_Tissue" = "kidney_cancer_tissue",
      "Tissue_Subtype" = "na_tissue_subtype",
      "FFPE" = "false_ffpe",
      "NeoICI_Rx" = "none_neoici_rx",
      "Prior_ICI_Rx" = "none_prior_ici_rx",
      "Prior_Rx" = "none_prior_rx",
      "Subsq_Rx" = "none_subsq_rx",
      "Subsq_ICI_Rx" = "none_subsq_ici_rx",
      "TCGA_Study" = "RCC"
    ) %>%
    tidyr::pivot_longer(-sample_id, names_to = "parent_tag", values_to = "tag_name")%>%
    dplyr::inner_join(tags, by = "tag_name") %>%
    dplyr::select("tag_id", "sample_id") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    samples_to_tags,
    "syn60530047",
    "samples_to_tags"
  )

}
