edges_tcga <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  nodes <-
    synapse_csv_id_to_tbl(syn, "syn51082227") %>%
    dplyr::select(
      "node_name" = "name",
      "node_id" = "id"
    )

  ecn_stratified_edges <- synapse_feather_id_to_tbl(syn, "syn26067672")
  ecn_immune_subtype_edges <- synapse_feather_id_to_tbl(syn, "syn23538678")
  ecn_tcga_study_edges <- synapse_feather_id_to_tbl(syn, "syn23538697")
  ecn_tcga_subtype_edges <- synapse_feather_id_to_tbl(syn, "syn23538713")

  ecn_edges <- dplyr::bind_rows(
    ecn_stratified_edges,
    ecn_immune_subtype_edges,
    ecn_tcga_study_edges,
    ecn_tcga_subtype_edges
  )

  ci_immune_subtype_edges <- synapse_feather_id_to_tbl(syn, "syn23538718")
  ci_tcga_study_edges <- synapse_feather_id_to_tbl(syn, "syn23538720")
  ci_tcga_subtype_edges <- synapse_feather_id_to_tbl(syn, "syn23538725")

  ci_edges <- dplyr::bind_rows(
    ci_immune_subtype_edges,
    ci_tcga_study_edges,
    ci_tcga_subtype_edges
  )

  edges <-
    dplyr::bind_rows(ecn_edges, ci_edges) %>%
    dplyr::select("name", "score", "label", "node1", "node2") %>%
    dplyr::inner_join(nodes, by = c("node1" = "node_name")) %>%
    dplyr::rename("node_1_id" = "node_id") %>%
    dplyr::inner_join(nodes, by = c("node1" = "node_name")) %>%
    dplyr::rename("node_2_id" = "node_id") %>%
    dplyr::select(-c("node1", "node2")) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "edges"
    )

  readr::write_csv(edges, "synapse_storage_manifest.csv")

}
