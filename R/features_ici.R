features_germline <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  features <-

    synapse_feather_id_to_tbl(syn, "syn25981545") %>%  #REPLACE

    dplyr::rename("feature_class" = "class") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "features"
    )

  synapse_store_table_as_csv(
    syn,
    features,
    "syn51613665",
    "features"
  )

}
