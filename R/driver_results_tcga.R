driver_results_tcga <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  datasets <-
    synapse_csv_id_to_tbl(syn, "syn51132398") %>%
    dplyr::select(
      "dataset_name" = "name",
      "dataset_id" = "id"
    )

  features <-
    synapse_csv_id_to_tbl(syn, "syn50944340") %>%
    dplyr::filter(.data$feature_class != "Clinical") %>%
    dplyr::select(
      "feature_name" =  "name",
      "feature_id" = "id"
    )

  tags <-
    synapse_csv_id_to_tbl(syn, "syn51080176") %>%
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )

  mutations <-
    synapse_csv_id_to_tbl(syn,  "syn51068962") %>%
    dplyr::select(
      "mutation_name" = "name",
      "mutation_id" = "id"
    )

  driver_results_label_to_hgnc <- function(label) {
    hgnc <- label %>% stringi::stri_extract_first(regex = "^[\\w\\s\\(\\)\\*\\-_\\?\\=]{1,}(?!=;)")
    return(ifelse(
      !identical(hgnc, "NA") & !is.na(hgnc),
      hgnc,
      NA
    ))
  }

  driver_results <-
    synapse_rds_id_to_tbl(syn,  "syn22126068") %>%
    dplyr::select(
      "label",
      "feature_name" = "metric",
      "tag_name" = "group2",
      "fold_change",
      "log10_p_value" = "log10_pvalue",
      "log10_fold_change",
      "p_value" = "pvalue",
      "n_wildtype" = "n_wt",
      "n_mutants" = "n_mut"
    ) %>%
    dplyr::mutate(
      "gene_mutation" = driver_results_label_to_hgnc(.data$label)
    ) %>%
    tidyr::separate(
      "gene_mutation",
      into = c("hgnc", "mutation_code"),
      sep = "\\s",
      remove = TRUE
    ) %>%
    dplyr::mutate(
      "mutation_code" = ifelse(
        is.na(.data$mutation_code),
        "(NS)",
        .data$mutation_code
      ),
      "mutation_name" = stringr::str_c(.data$hgnc, ":", .data$mutation_code),
      "feature_name" = stringr::str_replace_all(.data$feature_name, "[\\.]", "_"),
      "tag_name" = stringr::str_replace_all(.data$tag_name, "[\\.]", "_"),
      "tag_name" = stringr::str_replace_all(.data$tag_name, "-", "_"),
      "tag_name" = stringr::str_replace_all(.data$tag_name, ":", "_"),
      "tag_name" = stringr::str_remove_all(.data$tag_name, "[[:space:]]"),
      "dataset_name" = "TCGA"
    ) %>%
    dplyr::select(-c("label", "mutation_code", "hgnc")) %>%
    dplyr::inner_join(mutations, by = "mutation_name") %>%
    dplyr::inner_join(features, by = "feature_name") %>%
    dplyr::inner_join(tags, by = "tag_name") %>%
    dplyr::inner_join(datasets, by = "dataset_name") %>%
    dplyr::select(-c(
      "mutation_name", "feature_name", "tag_name", "dataset_name"
    )) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "driver_results"
    )

  driver_results_1 <- driver_results %>%
    dplyr::slice(1:400000)

  driver_results_2 <- driver_results %>%
    dplyr::slice(400001:800000)

  driver_results_3 <- driver_results %>%
    dplyr::slice(800001:dplyr::n())

  synapse_store_table_as_csv(
    syn,
    driver_results_1,
    "syn51082334",
    "driver_results"
  )

  synapse_store_table_as_csv(
    syn,
    driver_results_2,
    "syn51750853",
    "driver_results"
  )

  synapse_store_table_as_csv(
    syn,
    driver_results_3,
    "syn51750854",
    "driver_results"
  )

}
