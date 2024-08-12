cells_bi <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  cells <- synapse_tsv_id_to_tbl(syn, "syn60543861")

  cells <- cells %>%
    dplyr::filter(cell_type_iatlas != "Misc/Undetermined")%>%
    dplyr::select(
      "name" = "NAME",
      "cell_type" = "cell_type_iatlas"
    ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "cells"
    )

  synapse_store_table_as_csv(
    syn,
    cells,
    "syn60530040",
    "cells"
  )

}
