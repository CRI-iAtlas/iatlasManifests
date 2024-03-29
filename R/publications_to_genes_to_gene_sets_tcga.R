publications_to_genes_to_gene_sets_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  genes <-
    synapse_csv_id_to_tbl(syn, "syn50896922") %>%
    dplyr::select(
      "entrez" = "entrez_id",
      "gene_id" = "id"
    )

  gene_sets <-
    synapse_csv_id_to_tbl(syn, "syn51034026") %>%
    dplyr::select(
      "gene_set_name" = "name",
      "gene_set_id" = "id"
    )

  publications <-
    synapse_csv_id_to_tbl(syn, "syn51080887") %>%
    dplyr::select(
      "pubmed_id",
      "publication_id" = "id"
    )

  publications_to_genes_to_gene_sets <-
    synapse_feather_id_to_tbl(syn,  "syn23518445") %>%
    dplyr::mutate(
      "pubmed_id" = as.integer(.data$pubmed_id),
      "gene_set_name" = "immunomodulator"
    ) %>%
    dplyr::inner_join(publications, by = "pubmed_id") %>%
    dplyr::inner_join(genes, by = "entrez") %>%
    dplyr::inner_join(gene_sets, by = "gene_set_name") %>%
    dplyr::select(-c("pubmed_id", "pubmed_id",  "gene_set_name")) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "publications_to_genes_to_gene_sets"
    )

  synapse_store_table_as_csv(
    syn,
    publications_to_genes_to_gene_sets,
    "syn51080925",
    "publications_to_genes_to_gene_sets"
  )

}

