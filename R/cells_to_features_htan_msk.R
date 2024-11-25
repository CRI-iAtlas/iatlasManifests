cells_to_features_htan_msk <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  cells <- synapse_csv_id_to_tbl(syn, "syn53701021") %>%
    dplyr::select(
      "Cell",
      "umap_1",
      "umap_2"
    )

  cells_ids <- synapse_csv_id_to_tbl(syn, "syn53701038") %>%
    dplyr::select(
      "Cell" = "name",
      "cell_id" = "id"
    )

  features_ids <- synapse_csv_id_to_tbl(syn, "syn53701249")%>%
    dplyr::select(
      "feature_name"  = "name",
      "feature_id" = "id"
    )

  cells_to_features <- cells %>%
    tidyr::pivot_longer(-Cell, names_to = "feature_name", values_to = "feature_value") %>%
    dplyr::inner_join(cells_ids, by = "Cell") %>%
    dplyr::inner_join(features_ids, by = "feature_name") %>%
    dplyr::select(
      "cell_id",
      "feature_id",
      "feature_value"
    ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "cells_to_features"
    )

  synapse_store_table_as_csv(
    syn,
    cells_to_features,
    "syn53701290",
    "cells_to_features"
  )

}
