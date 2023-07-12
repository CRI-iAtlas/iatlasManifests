edges_pcawg <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  nodes <-
    synapse_csv_id_to_tbl(syn, "syn51095801") %>%
    dplyr::select(
      "node_name" = "name",
      "node_id" = "id"
    )

  ecn_immune_subtype_edges <-  synapse_feather_id_to_tbl(syn, "syn23538633")
  ecn_pcawg_study_edges <- synapse_feather_id_to_tbl(syn, "syn23538636")
  ci_immune_subtype_edges <- synapse_feather_id_to_tbl(syn, "syn23538627")
  ci_pcawg_study_edges <- synapse_feather_id_to_tbl(syn, "syn23538630")

  edges <-
    dplyr::bind_rows(
      ecn_immune_subtype_edges,
      ecn_pcawg_study_edges,
      ci_immune_subtype_edges,
      ci_pcawg_study_edges
    ) %>%
    dplyr::select("name", "score", "label", "node1", "node2") %>%
    dplyr::inner_join(nodes, by = c("node1" = "node_name")) %>%
    dplyr::rename("node_1_id" = "node_id") %>%
    dplyr::inner_join(nodes, by = c("node2" = "node_name")) %>%
    dplyr::rename("node_2_id" = "node_id") %>%
    dplyr::select(-c("node1", "node2")) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "edges"
    )

  synapse_store_table_as_csv(
    syn,
    edges,
    "syn51095812",
    "edges"
  )

}
