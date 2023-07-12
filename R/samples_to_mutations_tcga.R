samples_to_mutations_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  mutations <-
    synapse_csv_id_to_tbl(syn,  "syn51068962") %>%
    dplyr::select(
      "mutation_name" = "name",
      "mutation_id" = "id"
    )

  samples <-
    synapse_csv_id_to_tbl(syn, "syn50896891") %>%
    dplyr::select(
      "sample_name" = "name",
      "sample_id" = "id"
    )

  samples_to_mutations <-
    synapse_feather_id_to_tbl(syn, "syn22131029") %>%
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
    )


  samples_to_mutations1 <- samples_to_mutations %>%
    dplyr::slice(1:1000000)

  samples_to_mutations2 <- samples_to_mutations %>%
    dplyr::slice(1000001:2000000)

  samples_to_mutations3 <- samples_to_mutations %>%
    dplyr::slice(2000001:3000000)

  samples_to_mutations4 <- samples_to_mutations %>%
    dplyr::slice(3000001:4000000)

  samples_to_mutations5 <- samples_to_mutations %>%
    dplyr::slice(4000001:5000000)

  samples_to_mutations6 <- samples_to_mutations %>%
    dplyr::slice(5000001:6000000)

  samples_to_mutations7 <- samples_to_mutations %>%
    dplyr::slice(6000001:dplyr::n())


  synapse_store_table_as_csv(
    syn,
    samples_to_mutations1,
    "syn51745979",
    "samples_to_mutations"
  )

  synapse_store_table_as_csv(
    syn,
    samples_to_mutations2,
    "syn51746485",
    "samples_to_mutations"
  )

  synapse_store_table_as_csv(
    syn,
    samples_to_mutations3,
    "syn51746492",
    "samples_to_mutations"
  )

  synapse_store_table_as_csv(
    syn,
    samples_to_mutations4,
    "syn51750914",
    "samples_to_mutations"
  )

  synapse_store_table_as_csv(
    syn,
    samples_to_mutations5,
    "syn51750915",
    "samples_to_mutations"
  )

  synapse_store_table_as_csv(
    syn,
    samples_to_mutations6,
    "syn51750916",
    "samples_to_mutations"
  )

  synapse_store_table_as_csv(
    syn,
    samples_to_mutations7,
    "syn51750917",
    "samples_to_mutations"
  )

}



