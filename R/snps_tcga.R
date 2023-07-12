snps_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  snps <-

    synapse_feather_id_to_tbl(syn,  "syn24202044") %>% #REPLACE

    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "snps"
    )

  synapse_store_table_as_csv(
    syn,
    snps,
    "syn51440472",
    "snps"
  )

}

