tags_porter <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  clinical_data <- synapse_csv_id_to_tbl(syn, "syn54031467")





  tags <-

    synapse_feather_id_to_tbl(syn, "syn25999174") %>% # REPLACE

    dplyr::select(
      "name",
      "short_display",
      "long_display",
      "color",
      "description" = "characteristics",
      "tag_type" = "type",
      "order"
    ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "tags"
    )

  synapse_store_table_as_csv(
    syn,
    tags,
    "syn51613679",
    "tags"
  )


}
