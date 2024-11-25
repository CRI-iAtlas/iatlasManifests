cells_shiao <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  cells <- synapse_csv_id_to_tbl(syn, "syn55273095")

  cells <- cells %>%
    dplyr::mutate( #we need to first clean up the cell names
      "cell_type" = dplyr::case_when(
        cell_type == "Tcell" ~ "T cell",
        cell_type == "Bcell" ~ "B cell",
        cell_type == "myeloid" ~ "myeloid cell",
        cell_type == "mastcell" ~ "mast cell"
      )
    ) %>%
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
    "syn58445190",
    "cells"
  )

}
