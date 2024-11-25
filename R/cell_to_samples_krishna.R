cells_to_samples_krishna <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  cells <- synapse_tsv_id_to_tbl(syn, "syn59205039") %>% #original data
    dplyr::select(
      "cell_name" = "cell",
      "Sample2"
    ) %>%
    dplyr::mutate(
      sample_name = paste0("Krishna_ccRCC_", Sample2),
    )

  cells_ids <- synapse_csv_id_to_tbl(syn, "syn59424211") %>%
    dplyr::select(
      "cell_name" = "name",
      "cell_id" = "id"
    )

  samples_ids <- synapse_csv_id_to_tbl(syn, "syn59204288")%>%
    dplyr::select(
      "sample_name"  = "name",
      "sample_id" = "id"
    )

  cells_to_samples <- cells %>%
    dplyr::inner_join(cells_ids, by = "cell_name") %>%
    dplyr::inner_join(samples_ids, by = "sample_name") %>%
    dplyr::select(
      "cell_id",
      "sample_id"
    ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "cells_to_samples"
    )

  synapse_store_table_as_csv(
    syn,
    cells_to_samples,
    "syn59424220",
    "cells_to_samples"
  )

}
