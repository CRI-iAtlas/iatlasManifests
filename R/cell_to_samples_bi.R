cells_to_samples_bi <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  cells <- synapse_tsv_id_to_tbl(syn, "syn60543861") %>% #original data
    dplyr::select(
      "cell_name" = "NAME",
      "biosample_id"
    ) %>%
    dplyr::mutate(
      "sample_name" = paste0("Bi_ccRCC_", gsub("_scRNA", "", biosample_id))
    )

  cells_ids <- synapse_csv_id_to_tbl(syn, "syn60543893") %>%
    dplyr::select(
      "cell_name" = "name",
      "cell_id" = "id"
    )

  samples_ids <- synapse_csv_id_to_tbl(syn, "syn60531805")%>%
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
    "syn60530041",
    "cells_to_samples"
  )

}
