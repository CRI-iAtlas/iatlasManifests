cells_to_genes_htan_vanderbilt <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()
#TODO: store new genes

    #get cell ids
    cells_ids <- synapse_csv_id_to_tbl(syn, "syn53701120") %>%
      dplyr::select(
        "Cell" = "name",
        "cell_id" = "id"
      )

    #get gene info
    tcga_genes <- synapse_csv_id_to_tbl(syn, "syn50896922")
    htan_genes <- synapse_csv_id_to_tbl(syn, "syn53710212")

    #we will only add genes that are already in the iAtlas database
    genes_to_add <- tcga_genes[tcga_genes$hgnc_id %in% htan_genes$feature_name,  c("hgnc_id", "id")]

    #get expression values
    htan_expr <- synapse_csv_id_to_tbl(syn, "syn53710204")

    cells_to_genes <- htan_expr %>%
      dplyr::rename("Cell" = "...1") %>%
      dplyr::select(Cell, dplyr::all_of(genes_to_add$hgnc_id)) %>%
      dplyr::inner_join(cells_ids, by = "Cell") %>%
      tidyr::pivot_longer(-c(Cell, cell_id), names_to = "hgnc_id", values_to = "single_cell_seq") %>%
      dplyr::filter(single_cell_seq != 0) %>%
      dplyr::inner_join(genes_to_add, by = "hgnc_id", relationship = "many-to-many") %>%
      dplyr::select(
        "cell_id",
        "gene_id" = "id",
        "single_cell_seq") %>%
      dplyr::mutate(
        "id" = uuid::UUIDgenerate(n = dplyr::n())
      )

  # synapse_store_table_as_csv(
  #   syn,
  #   cells_to_genes,
  #   "syn53708096",
  #   "cells_to_genes"
  # )
#we need to split this file in smaller files
  synapse_store_table_as_csv(
     syn,
     all_msk[1:2303667,],
     "syn53708096",
     "cells_to_genes"
     )

  synapse_store_table_as_csv(
    syn,
    all_msk[2303668:4607334,],
    "syn53909814",
    "cells_to_genes"
  )

  synapse_store_table_as_csv(
    syn,
    all_msk[4607335:6911001,],
    "syn53910296",
    "cells_to_genes"
  )

  synapse_store_table_as_csv(
    syn,
    all_msk[6911002:9214668,],
    "syn53910297",
    "cells_to_genes"
  )

  synapse_store_table_as_csv(
    syn,
    all_msk[9214669:11518333,],
    "syn53910366",
    "cells_to_genes"
  )

}
