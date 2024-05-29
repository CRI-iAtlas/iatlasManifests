samples_to_tags_li <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()


  #getting clinical data from the patient table
  patient_info <-
    syn$get("syn59942707") %>%
    purrr::pluck("path") %>%
    openxlsx::read.xlsx(., sheet = 1) %>%
    dplyr::as_tibble()

  colnames(patient_info) <- patient_info[1,]
  patient_info <- patient_info[-c(1,14:23),]
  patient_info <- patient_info %>%
    dplyr::select(
      "patient_key" = "Patient_ID",
      "stage",
      "metastases",
      "Histology"
    )

  # Biopsy site info from the obs table in the h5ad file
  li_obs <- synapse_tsv_id_to_tbl(syn, "syn60085493") %>%
    dplyr::select("orig.ident", "patient", "summaryDescription") %>%
    dplyr::distinct()


  #getting ids
  samples <-
    synapse_csv_id_to_tbl(syn, "syn60085711") %>%
    dplyr::select(
      "sample_name" = "name",
      "sample_id" = "id"
    ) %>%
    dplyr::mutate(
      "orig.ident" = gsub(".*_(.*)", "\\1", sample_name),
      "patient_key" = gsub(".*_(.*)_.*", "\\1", sample_name),
    )

  tags <-
    synapse_csv_id_to_tbl(syn, "syn53698018") %>% #msk tags
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
    dplyr::inner_join(patient_info, by = "patient_key") %>%
    dplyr::mutate(
      "Clinical_Stage" = dplyr::case_when(
        stage %in% c("1a", "1b") ~ "i_clinical_stage",
        stage %in% c("3a") ~ "iii_clinical_stage",
        is.na(stage) ~ "na_clinical_stage"
      ),
      "Metastasized" = dplyr::case_when(
        metastases == 0 ~ "false_metastasized",
        metastases == 1 ~ "true_metastasized"
      ),
      "TCGA_Subtype" = dplyr::if_else(
        Histology == "ccRCC",
        "RCC_ccRCC",
        "na_tcga_subtype"
      )
    ) %>%
    dplyr::inner_join(li_obs, by = "orig.ident") %>%
    dplyr::mutate(
      "Biopsy_Site" = dplyr::case_when(
        summaryDescription %in% c("Tumour-normal", "Normal kidney", "Tumour", "Fat") ~ "kidney_biopsy_site",
        summaryDescription %in% c("Metastasis", "Normal adrenal") ~ "adrenal_gland_biopsy_site",
        summaryDescription == "Blood" ~ "pbmc_biopsy_site",
        summaryDescription == "Thrombus" ~ "thrombus_biopsy_site"
      ),
      "Tumor_tissue_type" = dplyr::case_when(
        summaryDescription %in% c("Blood", "Normal adrenal", "Fat", "Normal kidney") ~ "normal_tumor_tissue_type",
        summaryDescription%in% c("Tumour", "Thrombus") ~ "primary_tumor_tissue_type",
        summaryDescription == "Metastasis" ~ "metastatic_tumor_tissue_type",
        summaryDescription == "Tumour-normal" ~ "normal_adjacent_tumor_tissue_type"
      )
    ) %>%
    dplyr::select(
      sample_id,
      Clinical_Stage,
      Metastasized,
      TCGA_Subtype,
      Biopsy_Site,
      Tumor_tissue_type
    ) %>%
    dplyr::mutate( #these categories are defined by the study protocol
      "Polyp_Histology" = "na_polyp_histology",
      "Response" = "na_response",
      "Clinical_Benefit" = "na_clinical_benefit",
      "Progression" = "na_progression",
      "Cancer_Tissue" = "kidney_cancer_tissue",
      "Tissue_Subtype" = "na_tissue_subtype",
      "FFPE" = "false_ffpe",
      "NeoICI_Rx" = "none_neoici_rx",
      "ICI_Pathway" = "none_ici_pathway",
      "ICI_Rx" = "none_ICI_Rx",
      "ICI_Target" = "none_ICI_Target",
      "Non_ICI_Rx" = "surgery_non_ici_rx",
      "Prior_ICI_Rx" = "none_prior_ici_rx",
      "Prior_Rx" = "none_prior_rx",
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
    "syn60157474",
    "samples_to_tags"
  )

}
