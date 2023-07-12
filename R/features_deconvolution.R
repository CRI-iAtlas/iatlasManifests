features_deconvolution <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  epic_features <- c(
    "EPIC_B_Cells",
    "EPIC_CAFs",
    "EPIC_CD4_T_Cells",
    "EPIC_CD8_T_Cells",
    "EPIC_Endothelial",
    "EPIC_Macrophages",
    "EPIC_NK_Cells",
    "EPIC_Other_Cells"
  ) %>%
    dplyr::tibble("name" = .) %>%
    dplyr::mutate("feature_class" = "Epic", unit = "Fraction")

  mpc_features <- c(
    "MCPcounter_T_Cells",
    "MCPcounter_Cd8_T_Cells",
    "MCPcounter_Cytotoxic_Lymphocytes",
    "MCPcounter_Nk_Cells",
    "MCPcounter_B_Lineage",
    "MCPcounter_Monocytic_Lineage",
    "MCPcounter_Myeloid_Dendritic_Cells",
    "MCPcounter_Neutrophils",
    "MCPcounter_Endothelial_Cells",
    "MCPcounter_Fibroblasts"
  ) %>%
    dplyr::tibble("name" = .) %>%
    dplyr::mutate("feature_class" = "MCPcounter", unit = "Score")


  features <-
    dplyr::bind_rows(epic_features, mpc_features) %>%
    dplyr::mutate("display" = stringr::str_replace_all(.data$name, "_", " ")) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "features"
    )

  synapse_store_table_as_csv(
    syn,
    features,
    "syn51088108",
    "features"
  )

}
