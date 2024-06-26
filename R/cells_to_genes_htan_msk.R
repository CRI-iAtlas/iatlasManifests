cells_to_genes_htan_msk <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()
#TODO: store new genes

    #get cell ids
    cells_ids <- synapse_csv_id_to_tbl(syn, "syn53701038") %>%
      dplyr::select(
        "Cell" = "name",
        "cell_id" = "id"
      )

    #get gene info
    tcga_genes <- synapse_csv_id_to_tbl(syn, "syn50896922")
    htan_genes <- synapse_csv_id_to_tbl(syn, "syn53707692")

    #we will only add genes that are already in the iAtlas database
    genes_to_add <- htan_genes[htan_genes$feature_name %in% tcga_genes$hgnc_id, c("gene_ids", "feature_name")] %>%
      dplyr::inner_join(tcga_genes, by = dplyr::join_by(feature_name == hgnc_id)) %>%
      dplyr::select(
        "gene_ids",
        "gene_id" = "id"
      )

    #get expression values
    htan_expr <- synapse_csv_id_to_tbl(syn, "syn53707807")

    cells_to_genes <- htan_expr %>%
      dplyr::select(Cell, dplyr::all_of(genes_to_add$gene_ids)) %>%
      dplyr::inner_join(cells_ids, by = "Cell") %>%
      tidyr::pivot_longer(-c(Cell, cell_id), names_to = "gene_ids", values_to = "single_cell_seq") %>%
      dplyr::filter(single_cell_seq != 0) %>%
      dplyr::inner_join(genes_to_add, by = "gene_ids", relationship = "many-to-many") %>%
      dplyr::select(
        "cell_id",
        "gene_id",
        "single_cell_seq") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

    #This data is too large and we'll save it in 10 different files

  nrow_files <- as.integer(nrow(cells_to_genes)/10)

  synapse_store_table_as_csv(
    syn,
    cells_to_genes[1:nrow_files,],
    "syn53708005",
    "cells_to_genes"
  )

  synapse_store_table_as_csv(
    syn,
    cells_to_genes[(nrow_files+1):(2*nrow_files),],
    "syn53892120",
    "cells_to_genes"
  )

  synapse_store_table_as_csv(
    syn,
    cells_to_genes[(2*nrow_files+1):(3*nrow_files),],
    "syn53892193",
    "cells_to_genes"
  )

  synapse_store_table_as_csv(
    syn,
    cells_to_genes[(3*nrow_files+1):(4*nrow_files),],
    "syn53892255",
    "cells_to_genes"
  )

  synapse_store_table_as_csv(
    syn,
    cells_to_genes[(4*nrow_files+1):(5*nrow_files),],
    "syn53892368",
    "cells_to_genes"
  )

  synapse_store_table_as_csv(
    syn,
    cells_to_genes[(5*nrow_files+1):(6*nrow_files),],
    "syn53896643",
    "cells_to_genes"
  )

  synapse_store_table_as_csv(
    syn,
    cells_to_genes[(6*nrow_files+1):(7*nrow_files),],
    "syn53896670",
    "cells_to_genes"
  )

  synapse_store_table_as_csv(
    syn,
    cells_to_genes[(7*nrow_files+1):(8*nrow_files),],
    "syn53896728",
    "cells_to_genes"
  )

  synapse_store_table_as_csv(
    syn,
    cells_to_genes[(8*nrow_files+1):(9*nrow_files),],
    "syn53896835",
    "cells_to_genes"
  )

  synapse_store_table_as_csv(
    syn,
    cells_to_genes[(9*nrow_files+1):nrow(cells_to_genes),],
    "syn53896891",
    "cells_to_genes"
  )

}
