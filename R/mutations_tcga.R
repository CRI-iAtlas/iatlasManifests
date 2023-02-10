mutations_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  genes <-
    synapse_csv_id_to_tbl(syn,  "syn50896922") %>%
    dplyr::select(
      "hgnc" = "hgnc",
      "genes_id" = "id"
    )

  mutations <-
    synapse_feather_id_to_tbl(syn, "syn22131029") %>%
    dplyr::slice(1:100) %>% #remove
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
      "name" = stringr::str_c(.data$hgnc, ":", .data$mutation_code),
      "mutation_type" = "driver mutation"
    ) %>%
    dplyr::inner_join(genes, by = "hgnc") %>%
    dplyr::select(-"hgnc") %>%
    tidyr::drop_na() %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "mutations"
    )

  readr::write_csv(mutations, "synapse_storage_manifest.csv")
}



