tcga_build_genes_to_samples_files <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  gene_types <- c(
    "cellimage_network",
    "extracellular_network",
    "io_target",
    "immunomodulator",
    "cibersort_gene",
    "mcpcounter_gene",
    "epic_gene",
  )


  samples <-
    synapse_csv_id_to_tbl(syn, "syn50896891") %>%
    dplyr::select(
      "sample_name" = "name",
      "sample_id" = "id"
    )

  genes <-
    synapse_csv_id_to_tbl(syn, "syn50896922") %>%
    dplyr::select(
      "entrez",
      "hgnc",
      "gene_id" = "id"
    )

  genes <- "syn22162918" %>%
    synapse_feather_id_to_tbl(.) %>%
    dplyr::filter(.data$gene_type %in% gene_types) %>%
    dplyr::pull("entrez") %>%
    unique() %>%
    sort()

  genes_to_samples <-
    synapse_feather_id_to_tbl(syn, "syn23560243") %>%
    dplyr::select("entrez", "sample", "rna_seq_expr") %>%
    tidyr::drop_na() %>%
    dplyr::filter(.data$entrez %in% genes)

  iatlas.data::synapse_store_feather_file(
    expression,
    "tcga_genes_to_samples.feather",
    "syn22125645"
  )

}
