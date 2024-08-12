cells_to_features_htan_msk <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  cells <- synapse_tsv_id_to_tbl(syn, "syn60254554") %>%
    dplyr::select(
      "NAME",
      "umap_1",
      "umap_2"
    )

  cells_ids <- synapse_csv_id_to_tbl(syn, "syn60543893") %>%
    dplyr::select(
      "NAME" = "name",
      "cell_id" = "id"
    )

  features_ids <- synapse_csv_id_to_tbl(syn, "syn53701249")%>%
    dplyr::select(
      "feature_name"  = "name",
      "feature_id" = "id"
    )

  cells_to_features <- cells %>%
    tidyr::pivot_longer(-NAME, names_to = "feature_name", values_to = "feature_value") %>%
    dplyr::inner_join(cells_ids, by = "NAME") %>%
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
    "syn60530042",
    "cells_to_features"
  )

}
