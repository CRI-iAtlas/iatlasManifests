nodes_to_tags_tcga <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  nodes <-
    synapse_csv_id_to_tbl(syn, "syn51082227") %>%
    dplyr::select(
      "node_name" = "name",
      "node_id" = "id"
    )

  tags <-
    synapse_csv_id_to_tbl(syn, "syn51080176") %>%
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )

  ecn_stratified_nodes_to_tags <-
    synapse_feather_id_to_tbl(syn, "syn26067676") %>%
    tidyr::separate_rows("tag", sep = ":")

  ecn_immune_subtype_nodes_to_tags <-
    synapse_feather_id_to_tbl(syn, "syn23538679")

  ecn_tcga_study_nodes_to_tags <-
    synapse_feather_id_to_tbl(syn, "syn23538696")

  ecn_tcga_subtype_nodes_to_tags <-
    synapse_feather_id_to_tbl(syn, "syn23538712")

  ecn_nodes_to_tags <-
    dplyr::bind_rows(
      ecn_stratified_nodes_to_tags,
      ecn_immune_subtype_nodes_to_tags,
      ecn_tcga_study_nodes_to_tags,
      ecn_tcga_subtype_nodes_to_tags
    )

  ci_immune_subtype_nodes_to_tags <-
    synapse_feather_id_to_tbl(syn, "syn23538719")

  ci_tcga_study_nodes_to_tags <-
    synapse_feather_id_to_tbl(syn,  "syn23538721")

  ci_tcga_subtype_nodes_to_tags <-
    synapse_feather_id_to_tbl(syn, "syn23538726")

  ci_nodes_to_tags <-
    dplyr::bind_rows(
      ci_immune_subtype_nodes_to_tags,
      ci_tcga_study_nodes_to_tags,
      ci_tcga_subtype_nodes_to_tags
    )

  nodes_to_tags <-
    dplyr::bind_rows(ecn_nodes_to_tags, ci_nodes_to_tags) %>%
    dplyr::select("node_name" = "name", "tag_name" = "tag") %>%
    dplyr::inner_join(tags, by = "tag_name") %>%
    dplyr::inner_join(nodes, by = "node_name") %>%
    dplyr::select(-c("tag_name", "node_name")) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "nodes_to_tags"
    )

  readr::write_csv(nodes_to_tags, "synapse_storage_manifest.csv")

}
