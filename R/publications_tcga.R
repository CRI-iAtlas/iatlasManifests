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
      "name" = "title",
      "do_id",
      "pubmed_id",
      "journal",
      "first_author_last_name",
      "publicaiton_link" = "link",
      "year"
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "publications"
    )

  readr::write_csv(publications, "synapse_storage_manifest.csv")

}

