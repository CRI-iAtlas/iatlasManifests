#We deleted the cells to genes tables, as they are not being currently using in the app and their size was causing issues in the database deployment
#cells_to_genes_krishna <- function() {
#
#   require(magrittr)
#   require(rlang)
#   syn <- create_synapse_login()
#
#   #get cell ids
#   cells_ids <- synapse_csv_id_to_tbl(syn, "syn59424211") %>%
#     dplyr::select(
#       "Cell" = "name",
#       "cell_id" = "id"
#     )
#
#   #get gene info
#   genes_to_add <- synapse_csv_id_to_tbl(syn, "syn50896922")
#
#   #the expression file is large, we will need to read the data in chunks
#   folders_ids <- c("syn59473722", "syn59473723", "syn59473725", "syn59473729", "syn59473730",
#                    "syn59473731", "syn59473732", "syn59473733", "syn59473734", "syn59473735")
#
#   #entity <- syn$get("syn59473653")
#   chunk_size <- 11077005
#
#   for(i in 0:9){
#     print(i)
#     print(folders_ids[i+1])
#
#     #get expression values
#     cells_to_genes <- data.table::fread(entity$path, nrows = chunk_size, skip = chunk_size*i, col.names = c("Row", "Column", "Value")) %>%
#       dplyr::filter(Column %in% genes_to_add$hgnc_id) %>%
#       dplyr::filter(Value>0) %>%
#       dplyr::select(
#         "Cell" = "Row",
#         "hgnc_id" = "Column",
#         "single_cell_seq" = "Value"
#       ) %>%
#       dplyr::inner_join(cells_ids, by = "Cell") %>%
#       dplyr::inner_join(genes_to_add, by = "hgnc_id", relationship = "many-to-many") %>%
#       dplyr::select(
#         "cell_id",
#         "gene_id" = "id",
#         "single_cell_seq") %>%
#       dplyr::mutate(
#         "id" = uuid::UUIDgenerate(n = dplyr::n())
#       )
#     print("uploading file")
#     #we need to split this file in smaller files
#     synapse_store_table_as_csv(
#       syn,
#       cells_to_genes,
#       folders_ids[i+1],
#       "cells_to_genes"
#     )
#
#     #remove(list="cells_to_genes")
#   }
#
# }
