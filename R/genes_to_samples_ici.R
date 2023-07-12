genes_to_samples_ici <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

    rna_seq <-
      synapse_feather_id_to_tbl(syn, "syn25981839")


    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "genes_to_samples"
    )

  synapse_store_table_as_csv(
    syn,
    genes_to_samples,
    "syn51589330",
    "genes_to_samples"
  )

}
