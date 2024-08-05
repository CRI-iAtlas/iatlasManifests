cells_to_features_shiao <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  cells <- synapse_csv_id_to_tbl(syn, "syn55273095") %>% #original data
    dplyr::select(
      "cell_name" = "...1",
      "umap_1" = "umap1",
      "umap_2" = "umap2"
    )

  cells_ids <- synapse_csv_id_to_tbl(syn, "syn58445516") %>%
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
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "cells_to_features"
    )

  synapse_store_table_as_csv(
    syn,
    cells_to_features,
    "syn58452239",
    "cells_to_features"
  )

}
