cohorts_htan_msk <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  parent_tags <-
    #from samples to tags
    c("gender",
      "race",
      "ethnicity",
      "Sample_Collection_Timepoint",
      "ICI_Rx",
      "ICI_Pathway",
      "ICI_Target",
      "Non_ICI_Rx",
      "NeoICI_Rx",
      "Prior_Rx",
      "Prior_ICI_Rx",
      "Subsq_Rx",
      "Subsq_ICI_Rx",
      "Cancer_Tissue",
      "Tissue_Subtype",
      "Metastasized",
      "Clinical_Stage",
      "Biopsy_Site",
      "Tumor_tissue_type",
      "FFPE",
      "Responder",
      "Response",
      "Polyp_Histology",
      "Clinical_Benefit",
      "Progression",
      "TCGA_Study",
      "TCGA_Subtype")

  tags <-
    synapse_csv_id_to_tbl(syn, "syn53698018") %>% #msk tags
    dplyr::filter(name %in% parent_tags) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn53697423") %>% #vanderbilt tags
        dplyr::filter(name %in% parent_tags)
    )%>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn51613683") %>% #ici tags
        dplyr::filter(name %in% parent_tags)
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn51080176") %>% #add tags from TCGA
        dplyr::filter(name %in% parent_tags)
    ) %>%
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )

  datasets <-
    synapse_csv_id_to_tbl(syn, "") %>% #UPDATE
    dplyr::select(
      "dataset_name" = "name",
      "dataset_id" = "id"
    )

  cohorts <-
    tidyr::crossing(
      tag_name = tags$tag_name,
      dataset_name = datasets$dataset_name) %>%
    dplyr::mutate(
      name = paste(dataset_name, tag_name, sep = "_")
    ) %>%
    dplyr::bind_rows(
      data.frame(
        name = datasets$dataset_name,
        dataset_name = datasets$dataset_name,
        tag_name = NA
      )
    ) %>%
    dplyr::left_join(tags, by = "tag_name") %>%
    dplyr::inner_join(datasets, by = "dataset_name") %>%
    dplyr::select(-c("tag_name", "dataset_name"),
                  cohort_tag_id = tag_id) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    cohorts,
    "", #update
    "cohorts"
  )

}

