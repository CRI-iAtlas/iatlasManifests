mutations_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  genes <-
    synapse_csv_id_to_tbl(syn,  "syn50896922") %>%
    dplyr::select(
      "hgnc" = "hgnc_id",
      "gene_id" = "id"
    )

  mutation_type_id <-
    synapse_csv_id_to_tbl(syn,  "syn51671620") %>%
    dplyr::pull("id")

  mutations <-
    synapse_feather_id_to_tbl(syn, "syn22131029") %>%
    dplyr::rename("sample_name" = "ParticipantBarcode") %>%
    tidyr::pivot_longer(
      -"sample_name",
      values_to = "status",
      names_to = "mutation"
    ) %>%
    dplyr::select(-c("sample_name", "status")) %>%
    tidyr::separate(
      "mutation",
      into = c("hgnc", "mutation_code"),
      sep = " ",
      fill = "right"
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      "mutation_code" = dplyr::if_else(
        is.na(.data$mutation_code),
        "(NS)",
        .data$mutation_code
      ),
      "mutation_code" = dplyr::if_else(
        .data$mutation_code == "NA",
        "(NA)",
        .data$mutation_code
      ),
      "name" = stringr::str_c(.data$hgnc, ":", .data$mutation_code),
      "mutation_type_id" =  mutation_type_id
    ) %>%
    dplyr::inner_join(genes, by = "hgnc") %>%
    dplyr::select(-"hgnc") %>%
    tidyr::drop_na() %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "mutations"
    )

  synapse_store_table_as_csv(
    syn,
    mutations,
    "syn51068961",
    "mutations"
  )
}

mutations_tcga2 <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  mutations <-
    synapse_csv_id_to_tbl(syn,  "syn51068962")

  synapse_store_table_as_csv(
    syn,
    mutations,
    "syn51068961",
    "mutations"
  )


}



