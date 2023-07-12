copy_number_results_tcga <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  dataset_id <-
    synapse_csv_id_to_tbl(syn, "syn51132398") %>%
    dplyr::filter(.data$name == "TCGA") %>%
    dplyr::pull("id")

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

  genes <-
    synapse_csv_id_to_tbl(syn, "syn50896922") %>%
    dplyr::select(
      "hgnc" = "hgnc_id",
      "entrez" = "entrez_id",
      "gene_id" = "id"
    ) %>%
    dplyr::arrange(.data$entrez) %>%
    dplyr::group_by(.data$hgnc) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"entrez")

  paths <- c("syn21781426", "syn21781395", "syn21781409") %>%
    purrr::map(syn$get) %>%
    purrr::map(purrr::pluck, "path")

  for (path in paths) {
    load(path)
  }

  copy_number_results <-
    dplyr::bind_rows(
      immunetable,
      studytable,
      subtypetable
    ) %>%
    dplyr::select(
      "tag_name" = "Group",
      "hgnc" = "Gene",
      "feature_name" = "Metric",
      "direction" = "Direction",
      "mean_normal" = "Mean_Normal",
      "mean_cnv" = "Mean_CNV",
      "t_stat" = "T_stat",
      "p_value" = "Pvalue",
      "log10_p_value" = "Neg_log10_pvalue"
    ) %>%
    dplyr::mutate(
      "feature_name" = stringr::str_replace_all(.data$feature_name, "[\\.]", "_"),
      "tag_name" = stringr::str_replace_all(.data$tag_name, "[\\.]", "_"),
      "tag_name" = stringr::str_replace_all(.data$tag_name, "-", "_"),
      "tag_name" = stringr::str_replace_all(.data$tag_name, ":", "_"),
      "tag_name" = stringr::str_remove_all(.data$tag_name, "[[:space:]]")
    ) %>%
    dplyr::inner_join(genes, by = "hgnc") %>%
    dplyr::inner_join(features, by = "feature_name") %>%
    dplyr::inner_join(tags, by = "tag_name") %>%
    dplyr::select(-c("hgnc", "feature_name", "tag_name")) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "dataset_id" = dataset_id,
      "Component" = "copy_number_results"
    )

  copy_number_results1 <- copy_number_results %>%
    dplyr::slice(1:2000000)


  synapse_store_table_as_csv(
    syn,
    copy_number_results1,
    "syn51514780",
    "copy_number_results"
  )

}
