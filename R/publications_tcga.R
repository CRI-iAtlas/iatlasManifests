publications_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  immunomodulator_pubs <-
    synapse_feather_id_to_tbl(syn, "syn23518433")

  subtype_pubs <-
    synapse_feather_id_to_tbl(syn, "syn23545806")

  publications =
    dplyr::bind_rows(
      immunomodulator_pubs,
      subtype_pubs
    ) %>%
    dplyr::select(-"name") %>%
    dplyr::select(
      "title",
      "do_id",
      "pubmed_id",
      "journal",
      "first_author_last_name",
      "link",
      "year"
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "publications"
    )

  synapse_store_table_as_csv(
    syn,
    publications,
    "syn51080886",
    "publications"
  )

}

publications_tcga2 <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()


  publications <-
    synapse_csv_id_to_tbl(syn,"syn51080887") %>%
    dplyr::rename("title" = "name")

  synapse_store_table_as_csv(
    syn,
    publications,
    "syn51080886",
    "publications"
  )

}

