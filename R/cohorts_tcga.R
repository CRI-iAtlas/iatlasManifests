cohorts_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  datasets <-
    synapse_csv_id_to_tbl(syn, "syn51132398") %>%
    dplyr::select(
      "dataset_name" = "name",
      "dataset_id" = "id"
    )

  tags <-
    synapse_csv_id_to_tbl(syn, "syn51080176") %>%
    dplyr::select(
      "tag_name" = "name",
      "cohort_tag_id" = "id"
    )

  cohorts <- dplyr::tribble(
    ~name,                  ~dataset_name, ~tag_name,
    "TCGA",                 "TCGA",   NA,
    "TCGA_Immune_Subtype",  "TCGA",   "Immune_Subtype",
    "TCGA_TCGA_Subtype",    "TCGA",   "TCGA_Subtype",
    "TCGA_TCGA_Study",      "TCGA",   "TCGA_Study",
    "TCGA_gender",          "TCGA",   "gender",
    "TCGA_race",            "TCGA",   "race",
    "TCGA_ethnicity",       "TCGA",   "ethnicity",
  ) %>%
    dplyr::left_join(tags, by = "tag_name") %>%
    dplyr::inner_join(datasets, by = "dataset_name") %>%
    dplyr::select(-c("tag_name", "dataset_name")) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    cohorts,
    "syn51088083",
    "cohorts"
  )

}

