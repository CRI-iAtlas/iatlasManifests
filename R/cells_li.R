cells_li <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  cells <- synapse_tsv_id_to_tbl(syn, "syn60085493")

  cells <- cells %>%
    dplyr::select(
      "name" = "...1",
      "cell_type" = "cell_type_iatlas"
    ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    cells,
    "syn60085745",
    "cells"
  )

}
