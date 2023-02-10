mutations_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  mutations <-
    synapse_csv_id_to_tbl(syn,  "syn51068962") %>%
    dplyr::select(
      "mutation_name" = "name",
      "mutations_id" = "id"
    )

  samples <-
    synapse_csv_id_to_tbl(syn, "syn50896891") %>%
    dplyr::select(
      "sample_name" = "name",
      "samples_id" = "id"
    )

  samples_to_mutations <-
    synapse_feather_id_to_tbl(syn, "syn22131029") %>%
    dplyr::slice(1:100) %>%  # remove!!
    dplyr::rename("sample_name" = "ParticipantBarcode") %>%
    tidyr::pivot_longer(
      -"sample_name",
      values_to = "mutation_status",
      names_to = "mutation_name"
    ) %>%
    tidyr::separate(
      "mutation_name",
      into = c("hgnc", "mutation_code"),
      sep = " ",
      fill = "right",
      remove = F
    ) %>%
    dplyr::mutate(
      "mutation_code" = dplyr::if_else(
        is.na(.data$mutation_code),
        "(NS)",
        .data$mutation_code
      )
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      "mutation_name" = stringr::str_c(.data$hgnc, ":", .data$mutation_code)
    ) %>%
    dplyr::select(-c("hgnc", "mutation_code")) %>%
    dplyr::inner_join(mutations, by = "mutation_name") %>%
    dplyr::select(-"mutation_name") %>%
    dplyr::inner_join(samples, by = "sample_name") %>%
    dplyr::select(-"sample_name") %>%
    tidyr::drop_na() %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "samples_to_mutations"
    ) %>%
    dplyr::slice(1:100)  # remove!!


  readr::write_csv(samples_to_mutations, "synapse_storage_manifest.csv")
}



