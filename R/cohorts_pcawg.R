cohorts_pcawg <- function(){

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
    dplyr::bind_rows(
      synapse_csv_id_to_tbl(syn, "syn51080176"),
      synapse_csv_id_to_tbl(syn, "syn51088201")
    ) %>%
    dplyr::select(
      "tag_name" = "name",
      "cohort_tag_id" = "id"
    )

  cohorts <- dplyr::tribble(
    ~name,                  ~dataset_name, ~tag_name,
    "PCAWG",                "PCAWG",  NA,
    "PCAWG_Immune_Subtype", "PCAWG",  "Immune_Subtype",
    "PCAWG_PCAWG_Study",    "PCAWG",  "PCAWG_Study",
  ) %>%
    dplyr::left_join(tags, by = "tag_name") %>%
    dplyr::inner_join(datasets, by = "dataset_name") %>%
    dplyr::select(-c("tag_name", "dataset_name")) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "cohorts"
    )

  synapse_store_table_as_csv(
    syn,
    cohorts,
    "syn51095646",
    "cohorts"
  )

}

