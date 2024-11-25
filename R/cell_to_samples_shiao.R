cells_to_samples_shiao <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  cells <- synapse_csv_id_to_tbl(syn, "syn55273095") %>% #original data
    dplyr::select(
      "cell_name" = "...1",
      "patient_treatment"
    ) %>%
    dplyr::mutate(
      "sample_name" = paste0("Shiao_BRCA_", patient_treatment)
    )

  cells_ids <- synapse_csv_id_to_tbl(syn, "syn58445516") %>%
    dplyr::select(
      "cell_name" = "name",
      "cell_id" = "id"
    )

  samples_ids <- synapse_csv_id_to_tbl(syn, "syn58433237")%>%
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
    "syn58446189",
    "cells_to_samples"
  )

}
