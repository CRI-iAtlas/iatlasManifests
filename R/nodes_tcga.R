nodes_tcga <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  datasets <-
    synapse_csv_id_to_tbl(syn, "syn51080455") %>%
    dplyr::select(
      "dataset_name" = "name",
      "dataset_id" = "id"
    )

  genes <-
    synapse_csv_id_to_tbl(syn, "syn50896922") %>%
    dplyr::select(
      "entrez",
      "node_gene_id" = "id"
    )

  features <-
    synapse_csv_id_to_tbl(syn, "syn50944340") %>%
    dplyr::select(
      "feature_name" =  "name",
      "node_feature_id" = "id"
    )

  ecn_stratified_nodes <- synapse_feather_id_to_tbl(syn, "syn26067676")
  ecn_immune_subtype_nodes <- synapse_feather_id_to_tbl(syn, "syn23538679")
  ecn_tcga_study_nodes <- synapse_feather_id_to_tbl(syn, "syn23538696")
  ecn_tcga_subtype_nodes <- synapse_feather_id_to_tbl(syn, "syn23538712")

  ecn_nodes <-
    dplyr::bind_rows(
      ecn_stratified_nodes,
      ecn_immune_subtype_nodes,
      ecn_tcga_study_nodes,
      ecn_tcga_subtype_nodes
    ) %>%
    dplyr::mutate("network" = "Extracellular Network") %>%
    dplyr::select(
      "name", "network", "feature", "entrez", "score", "label", "dataset"
    )

  ci_immune_subtype_nodes <- synapse_feather_id_to_tbl(syn, "syn23538719")
  ci_tcga_study_nodes <- synapse_feather_id_to_tbl(syn, "syn23538721")
  ci_tcga_subtype_nodes <- synapse_feather_id_to_tbl(syn, "syn23538726")

  ci_nodes <- dplyr::bind_rows(
    ci_immune_subtype_nodes,
    ci_tcga_study_nodes,
    ci_tcga_subtype_nodes
  ) %>%
    dplyr::mutate("network" = "Cellimage Network") %>%
    dplyr::select(
      "name", "network", "feature", "entrez", "score", "dataset", "x", "y"
    )

  nodes <-
    dplyr::bind_rows(ecn_nodes, ci_nodes) %>%
    dplyr::rename(
      "feature_name" = "feature",
      "dataset_name" = "dataset"
    ) %>%
    dplyr::left_join(features, by = c("feature_name")) %>%
    dplyr::left_join(genes, by = c("entrez")) %>%
    dplyr::inner_join(datasets, by = c("dataset_name")) %>%
    dplyr::select(-c("feature_name", "entrez", "dataset_name")) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "nodes"
    )

  readr::write_csv(nodes, "synapse_storage_manifest.csv")

}
