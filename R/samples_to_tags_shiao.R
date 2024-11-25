samples_to_tags_shiao <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()


  # shiao treatment
  shiao_rx <- syn$get("syn58403343") %>%
    purrr::pluck("path") %>%
    openxlsx::read.xlsx(., sheet = 2) %>%
    dplyr::as_tibble() %>%
    dplyr::select(
      "Paper.Number", "Chemotherapy") %>%
    dplyr::mutate(
      "name" = dplyr::if_else( #we need to format the patient ID to match the ID in the h5ad, and also add study identifier
        nchar(.$Paper.Number) == 1,
        paste0("Shiao_BRCA_Patient0", Paper.Number),
        paste0("Shiao_BRCA_Patient", Paper.Number),
      )
    ) %>%
    dplyr::inner_join( # dictionary to convert post treatment annotation to tags
      synapse_csv_id_to_tbl(syn, "syn58895700"), by = dplyr::join_by(Chemotherapy == original_names)
    ) %>%
    dplyr::select(
      "patient_name" = "name",
      "Subsq_Rx" = "names"
    )

  # we will get the sample collection timepoint and response annotation from the obs in the h5ad file
  shiao_obs <- synapse_csv_id_to_tbl(syn, "syn55273095") %>%
    dplyr::mutate(
      "patient_name" = paste0("Shiao_BRCA_",  gsub("T[[:digit:]]", "", cohort)),
      "sample_name" = paste0("Shiao_BRCA_", patient_treatment)
    ) %>%
    dplyr::select("patient_name", "sample_name", "pCR", "treatment")  %>%
    dplyr::distinct() %>% #adding the subsq_treat info
    dplyr::inner_join(shiao_rx, by = "patient_name") %>%
    dplyr::mutate(
      "Responder" = dplyr::if_else(
        pCR == "R",
        "true_responder",
        "false_responder"
      ),
      "Sample_Treatment" = dplyr::case_when(
        treatment == "Base" ~ "pre_sample_treatment",
        treatment == "PD1" ~ "on_sample_treatment",
        treatment == "RTPD1" ~ "post_sample_treatment"
      )
    ) %>%
    dplyr::select(sample_name, Subsq_Rx, Responder, Sample_Treatment)

  #getting ids
  samples <-
    synapse_csv_id_to_tbl(syn, "syn58433237") %>%
    dplyr::select(
      "sample_name" = "name",
      "sample_id" = "id",
    )

  tags <-
    synapse_csv_id_to_tbl(syn, "syn53698018") %>% #msk tags
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn58896103") #shiao tags
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
  samples_to_tags <-samples %>%
    dplyr::inner_join(shiao_obs, by = "sample_name") %>%
    dplyr::mutate( #these categories are defined by the study protocol
      "gender" = "female",
      "race" = "na_race",
      "ethnicity" = "na_ethnicity",
      "Polyp_Histology" = "na_polyp_histology",
      "Tumor_tissue_type" = "primary_tumor_tissue_type",
      "Response" = "na_response",
      "Clinical_Benefit" = "na_clinical_benefit",
      "Progression" = "na_progression",
      "Biopsy_Site" = "breast_biopsy_site",
      "Cancer_Tissue" = "breast_cancer_tissue",
      "Clinical_Stage" = "ii_clinical_stage",
      "FFPE" = "false_ffpe",
      "Metastasized" = "false_metastasized",
      "Tissue_Subtype" = "na_tissue_subtype",
      "NeoICI_Rx" = "none_neoici_rx",
      "ICI_Pathway" = "pd1_ici_pathway",
      "ICI_Rx" = "pembro_ici_rx",
      "ICI_Target" = "pd1_ici_target",
      "Non_ICI_Rx" = "radiation_non_ici_rx",
      "Prior_ICI_Rx" = "none_prior_ici_rx",
      "Prior_Rx" = "none_prior_rx",
      "Subsq_ICI_Rx" = "none_subsq_ici_rx",
      "TCGA_Study" = "BRCA",
      "TCGA_Subtype" = "BRCA_TNBC"
    ) %>%
    dplyr::select(-sample_name) %>%
    tidyr::pivot_longer(-sample_id, names_to = "parent_tag", values_to = "tag_name")%>%
    dplyr::inner_join(tags, by = "tag_name") %>%
    dplyr::select("tag_id", "sample_id") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    samples_to_tags,
    "syn58913509",
    "samples_to_tags"
  )

}
