cells_to_features_li <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  cells <- synapse_tsv_id_to_tbl(syn, "syn59809830") %>% #original data
    dplyr::rename(
      "cell_name" = "...1"
    )

  cells_ids <- synapse_csv_id_to_tbl(syn, "syn60085813") %>%
    dplyr::select(
      "cell_name" = "name",
      "cell_id" = "id"
    )

  features_ids <- synapse_csv_id_to_tbl(syn, "syn53701249")%>%
    dplyr::select(
      "feature_name"  = "name",
      "feature_id" = "id"
    )

  cells_to_features <- cells %>%
    tidyr::pivot_longer(-cell_name, names_to = "feature_name", values_to = "feature_value") %>%
    dplyr::inner_join(cells_ids, by = "cell_name") %>%
    dplyr::inner_join(features_ids, by = "feature_name") %>%
    dplyr::select(
      "cell_id",
      "feature_id",
      "feature_value"
    ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    cells_to_features,
    "syn60085857",
    "cells_to_features"
  )

}
