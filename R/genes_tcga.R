genes_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  immunomodulators <- synapse_feather_id_to_tbl(syn, "syn23518460")
  io_targets <-
    synapse_feather_id_to_tbl(syn, "syn22151533") %>%
    dplyr::select(
      "entrez" = "Entrez ID",
      "io_landscape_name" = "Friendly Name",
      "gene_pathway" = "Pathway",
      "therapy_type" = "Therapy Type",
      "description" = "Description"
    ) %>%
    dplyr::group_by(.data$entrez) %>%
    dplyr::mutate("entrez" = as.integer(.data$entrez))

  hgnc_to_entrez <- synapse_feather_id_to_tbl(syn, "syn22240716")

  genes <-
    purrr::reduce(
      list(immunomodulators, io_targets),
      dplyr::full_join,
      by = "entrez"
    ) %>%
    dplyr::right_join(hgnc_to_entrez, by = "entrez") %>%
    dplyr::select(
      "entrez_id"  = "entrez",
      "hgnc_id" = "hgnc",
      dplyr::everything()
    ) %>%
    dplyr::mutate(
      "id" =  uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "genes"
    )

  synapse_store_table_as_csv(
    syn,
    genes,
    "syn50896918",
    "genes"
  )

}

