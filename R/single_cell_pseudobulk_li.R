single_cell_pseudobulk_li <- function(){
  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  dataset_id <- synapse_csv_id_to_tbl(syn, "syn60085138") %>%
    dplyr::pull(id)

  samples_ids <- synapse_csv_id_to_tbl(syn, "syn60085711")%>%
    dplyr::select(
      "sample_name"  = "name",
      "sample_id" = "id"
    ) %>%
    dplyr::mutate(
      "sample_key" = gsub(".*_(.*)", "\\1", sample_name)
    )

  pseudobulk <- synapse_csv_id_to_tbl(syn, "syn59882234") %>%
    tidyr::separate_wider_regex("...1", c(sample_key = ".*", "_", cell_type = ".*")) %>%
    tidyr::pivot_longer(-c("sample_key", "cell_type"), names_to = "feature_name", values_to =  "single_cell_seq_sum")

  #getting ids
  tcga_genes <- synapse_csv_id_to_tbl(syn, "syn50896922") %>%
    dplyr::select(
      "feature_name" = "hgnc_id",
      "gene_id" = "id"
    )


  single_cell_pseudobulk <- pseudobulk %>%
    dplyr::inner_join(samples_ids, by = "sample_key") %>%
    dplyr::inner_join(tcga_genes, by = "feature_name", relationship = "many-to-many") %>%
    dplyr::select(
      "sample_id",
      "cell_type",
      "gene_id",
      "single_cell_seq_sum"
    ) %>%
    dplyr::mutate(
      "dataset_id" = dataset_id,
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  #this dataset is too large for storage in Synapse, we need to split it into 4 files

  folders_ids <- c("syn60120084", "syn60120719", "syn60120724", "syn60120726")

  chunk_size <- nrow(single_cell_pseudobulk)/4

  for(i in 0:3){
    synapse_store_table_as_csv(
      syn,
      single_cell_pseudobulk[(1+chunk_size*i):(chunk_size*(i+1)),],
      folders_ids[i+1],
      "single_cell_pseudobulk"
    )
  }




}
