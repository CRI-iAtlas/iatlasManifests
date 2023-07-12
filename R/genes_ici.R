genes_ici <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  genes <-

    synapse_feather_id_to_tbl(syn, "syn25981546") %>% #REPLACE

    dplyr::rename(
      "entrez_id" = "entrez",
      "hgnc_id" = "hgnc"
    ) %>%
    dplyr::mutate(
      "id" =  uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "genes"
    )

  synapse_store_table_as_csv(
    syn,
    genes,
    "syn51589271",
    "genes"
  )

}

