cells_htan_msk <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  cells <- synapse_csv_id_to_tbl(syn, "syn53701021")

  cells <- cells %>%
    dplyr::mutate( #we need to first clean up the cell names
      "cell_type" = dplyr::case_when(
        cell_type == "T cells" ~ "T cell",
        cell_type == "Myeloid" ~ "myeloid cell",
        TRUE ~ cell_type
      )
    ) %>%
    dplyr::select(
      "name" = "Cell",
      "cell_type"
    ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "cells"
    )

  synapse_store_table_as_csv(
    syn,
    cells,
    "syn53701037",
    "cells"
  )

}
