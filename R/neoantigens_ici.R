neoantigens_ici <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  patients <-
    synapse_csv_id_to_tbl(syn, "syn51589396") %>%
    dplyr::select(
      "patient_name" =  "name",
      "patient_id" = "id"
    )

  genes <-
    dplyr::bind_rows(
      synapse_csv_id_to_tbl(syn, "syn51589283"),
      synapse_csv_id_to_tbl(syn, "syn50896922")
    ) %>%

    dplyr::select(
      "gene_name" = "hgnc_id",
      "gene_id" = "id"
    )


  neoantigens <-
    synapse_feather_id_to_tbl(syn,  "syn51658034") %>%
    dplyr::inner_join(patients, by = "patient_name") %>%
    dplyr::left_join(genes, by = "gene_name") %>%
    dplyr::select(-c("gene_name", "patient_name")) %>%
    dplyr::rename("neoantigen_gene_id" = "gene_id") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "neoantigens"
    )

  synapse_store_table_as_csv(
    syn,
    neoantigens,
    "syn51664483",
    "neoantigens"
  )

}

