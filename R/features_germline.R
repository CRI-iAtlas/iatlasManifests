features_germline <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  features <-

    synapse_feather_id_to_tbl(syn, "syn23651689") %>% #REPLACE
    dplyr::inner_join(
      synapse_feather_id_to_tbl(syn, "syn25170093"),
      by = "name"
    ) %>%
    dplyr::rename("feature_class" = "class",
                  "feature_germline_category" = "germline_category",
                  "feature_germline_category" = "germline_category") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    features,
    "syn51588915",
    "features"
  )

}
