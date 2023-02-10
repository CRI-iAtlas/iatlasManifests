tags_tcga <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  tags <-
    synapse_feather_id_to_tbl(syn, "syn23545011") %>%
    dplyr::select(
      "name",
      "short_display",
      "long_display",
      "color",
      "description" = "characteristics",
      "tag_type" = "type"
    ) %>%
    dplyr::arrange(.data$name) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "tags"
    )

  readr::write_csv(tags, "synapse_storage_manifest.csv")

}
