features_neoantigens <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  features <-
    synapse_feather_id_to_tbl(syn, "syn51658035") %>%
    dplyr::select(
      "name" = "feature_name",
      "display" = "feature_display",
      "feature_class" = "class"
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "features"
    )

  synapse_store_table_as_csv(
    syn,
    features,
    "syn51664489",
    "features"
  )

}
