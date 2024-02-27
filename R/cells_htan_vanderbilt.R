cells_htan_vanderbilt <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  cells <- synapse_csv_id_to_tbl(syn, "syn53701022")

  cells <- cells %>%
    dplyr::select(
      "name" = "...1",
      "cell_type"
    ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "cells"
    )

  synapse_store_table_as_csv(
    syn,
    cells,
    "syn53701116",
    "cells"
  )

}
