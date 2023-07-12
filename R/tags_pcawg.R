tags_pcawg <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  tags <-
    synapse_tsv_id_to_tbl(syn, "syn21785582") %>%
    dplyr::select("name" = "dcc_project_code") %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      "short_display" = .data$name,
      "long_display" = .data$name,
      "tag_type" = "group"
    ) %>%
    dplyr::add_row(
      name = "PCAWG_Study",
      short_display = "PCAWG Study",
      long_display = "PCAWG Study",
      "tag_type" = "parent_group"
    ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "tags"
    )

  readr::write_csv(tags, "synapse_storage_manifest.csv", na = "")

}
