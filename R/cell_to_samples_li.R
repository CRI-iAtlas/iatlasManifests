cells_to_samples_li <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  cells <- synapse_tsv_id_to_tbl(syn, "syn60085493") %>% #obs from h5ad
    dplyr::select(
      "cell_name" = "...1",
      "patient",
      "orig.ident"
    ) %>%
    dplyr::mutate(
      "sample_name" = paste("Li_ccRCC", patient, orig.ident, sep = "_"),
    )

  cells_ids <- synapse_csv_id_to_tbl(syn, "syn60085813") %>%
    dplyr::select(
      "cell_name" = "name",
      "cell_id" = "id"
    )

  samples_ids <- synapse_csv_id_to_tbl(syn, "syn60085711")%>%
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
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    cells_to_samples,
    "syn60085853",
    "cells_to_samples"
  )

}
