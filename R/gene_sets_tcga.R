gene_sets_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  wolf <- synapse_tsv_id_to_tbl(syn, "syn22240714")
  yasin <- synapse_tsv_id_to_tbl(syn, "syn22240715")

  genesets <- dplyr::tribble(
    ~name,                            ~display,
    "extracellular_network",          "Extra Cellular Network",
    "cellimage_network",              "Cellimage Network",
    "immunomodulator",                "Immunomodulator",
    "io_target",                      "IO Target",
    "potential_immunomodulator",      "Potential Immunomodulator",
    "cibersort_gene",                 "Cibersort Gene",
    "mcpcounter_gene",                "MCPcounter Gene",
    "epic_gene",                      "Epic Gene",
    "immune_subtype_classifier_gene", "Immune Subtype Classifier Gene"
  )

  gene_sets <-
    dplyr::bind_rows(wolf, yasin) %>%
    dplyr::select("name" = "GeneSet") %>%
    dplyr::distinct() %>%
    tidyr::drop_na() %>%
    dplyr::mutate(
      "name" = stringr::str_replace_all(.data$name, "[ \\.]", "_")
    ) %>%
    dplyr::mutate(
      "display" = stringr::str_to_title(stringr::str_replace_all(.data$name, "_", " "))
    ) %>%
    dplyr::arrange(.data$name) %>%
    dplyr::bind_rows(genesets, .) %>%
    dplyr::arrange(.data$name) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "gene_sets"
    )

  readr::write_csv(gene_sets, "synapse_storage_manifest.csv")
}



