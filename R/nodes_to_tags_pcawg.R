nodes_to_tags_pcawg <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  nodes <-
    synapse_csv_id_to_tbl(syn, "syn51095801") %>%
    dplyr::select(
      "node_name" = "name",
      "node_id" = "id"
    )

  tags <-
    dplyr::bind_rows(
      synapse_csv_id_to_tbl(syn, "syn51080176"),
      synapse_csv_id_to_tbl(syn, "syn51088201")
    ) %>%
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )

  ecn_immune_subtype_nodes_to_tags <-
    synapse_feather_id_to_tbl(syn, "syn23538632")

  ecn_pcawg_study_nodes_to_tags <-
    synapse_feather_id_to_tbl(syn, "syn23538635")

  ci_immune_subtype_nodes_to_tags <-
    synapse_feather_id_to_tbl(syn, "syn23538626")

  ci_pcawg_study_nodes_to_tags <-
    synapse_feather_id_to_tbl(syn, "syn23538628")

  nodes_to_tags <-
    dplyr::bind_rows(
      ecn_immune_subtype_nodes_to_tags,
      ecn_pcawg_study_nodes_to_tags,
      ci_immune_subtype_nodes_to_tags,
      ci_pcawg_study_nodes_to_tags
    ) %>%
    dplyr::select("node_name" = "name", "tag_name" = "tag") %>%
    dplyr::inner_join(tags, by = "tag_name") %>%
    dplyr::inner_join(nodes, by = "node_name") %>%
    dplyr::select(-c("tag_name", "node_name")) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "nodes_to_tags"
    )

  synapse_store_table_as_csv(
    syn,
    nodes_to_tags,
    "syn51095947",
    "nodes_to_tags"
  )

}
