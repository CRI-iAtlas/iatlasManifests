cells_to_samples_htan_vanderbilt <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  cells <- synapse_csv_id_to_tbl(syn, "syn53701022") %>%
    dplyr::select(
      "Cell" = "...1",
      "sample_name" = "sample_id"
    )

  cells_ids <- synapse_csv_id_to_tbl(syn, "syn53701120") %>%
    dplyr::select(
      "Cell" = "name",
      "cell_id" = "id"
    )

  samples_ids <- synapse_csv_id_to_tbl(syn, "syn53678312")%>%
    dplyr::select(
      "sample_name"  = "name",
      "sample_id" = "id"
    )

  cells_to_samples <- cells %>%
    dplyr::inner_join(cells_ids, by = "Cell") %>%
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
    "syn53701154",
    "cells_to_samples"
  )

}
