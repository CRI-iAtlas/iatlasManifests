cells_to_genes_shiao <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

    #get cell ids
    cells_ids <- synapse_csv_id_to_tbl(syn, "syn58445516") %>%
      dplyr::select(
        "Cell" = "name",
        "cell_id" = "id"
      )

    #get gene info
    genes_to_add <- synapse_csv_id_to_tbl(syn, "syn50896922")

    #the expression file is large, we will need to read the data in chunks
    folders_ids <- c("syn59059750", "syn59059753", "syn59059760", "syn59059761", "syn59059869",
                     "syn59059870", "syn59059871", "syn59059899", "syn59059900", "syn59059901",
                     "syn59068365", "syn59068497", "syn59069029", "syn59069030", "syn59069031",
                     "syn59069032", "syn59069033", "syn59069034", "syn59069035", "syn59069037",
                     "syn59069802", "syn59069803")

    entity <- syn$get("syn59059670")
    chunk_size <- 22500000

    for(i in 16:21){
      print(i)
      print(folders_ids[i+1])

      #get expression values
      cells_to_genes <- data.table::fread(entity.path, nrows = chunk_size, skip = chunk_size*i, col.names = c("Row", "Column", "Value")) %>%
        dplyr::filter(Column %in% genes_to_add$hgnc_id) %>%
        dplyr::filter(Value>0) %>%
        dplyr::select(
          "Cell" = "Row",
          "hgnc_id" = "Column",
          "single_cell_seq" = "Value"
        ) %>%
        dplyr::inner_join(cells_ids, by = "Cell") %>%
        dplyr::inner_join(genes_to_add, by = "hgnc_id", relationship = "many-to-many") %>%
        dplyr::select(
          "cell_id",
          "gene_id" = "id",
          "single_cell_seq") %>%
        dplyr::mutate(
          "id" = uuid::UUIDgenerate(n = dplyr::n())
        )
      print("uploading file")
      #we need to split this file in smaller files
      synapse_store_table_as_csv(
         syn,
         cells_to_genes,
         folders_ids[i+1],
         "cells_to_genes"
         )

      #remove(list="cells_to_genes")
    }

}
