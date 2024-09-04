cells_krishna <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  cells <- synapse_tsv_id_to_tbl(syn, "syn59205039")

  cells <- cells %>%
    dplyr::filter(!cluster_name %in% c("Ambiguous", "Ambiguous/Dead", "TAM/TCR (Ambiguos)")) %>%
    dplyr::mutate( #we need to first clean up the cell names
      "cell_type" = dplyr::case_when(
        cluster_name == "B cell" ~ "B cell",
        cluster_name == "Mast" ~ "mast cell",
        cluster_name == "CD45- ccRCC CA9+" ~ "tumor cell",
        cluster_name == "CD45- Myofibroblast" ~ "fibroblast",
        cluster_name == "CD45- PAX8+ renal epithelium" ~ "epithelium",
        cluster_name == "CD45- Vascular Endothelium" ~ "endothelium",
        cluster_name == "Megakaryocyte" ~ "megakaryocyte",
        cluster_name  %in% c("Conventional NK", "NK HSP+") ~ "NK",
        cluster_name %in% c("CD14+ Monocyte", "CD14+/CD16+ Monocyte") ~ "monocyte",
        cluster_name %in% c("CD4+ Treg",
                            "CD4+ Naive",
                            "CD4+ Effector",
                            "CD4+ Activated IEG",
                            "CD4+ Proliferating",
                            "CD8A+ Proliferating",
                            "CD8A+ Exhausted IEG",
                            "CD8A+ Exhausted",
                            "CD8A+ NK-like",
                            "CD8A+ Tissue-resident") ~ "T cell",
        cluster_name %in% c("cDC1", "cDC2", "pDC") ~ "dendritic cell",
        cluster_name %in% c("TAM HLAhi", "TAM HLAint", "TAM ISGhi", "TAM ISGint") ~ "macrophage"
      )
    ) %>%
    dplyr::select(
      "name" = "cell",
      "cell_type"
    ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "cells"
    )

  synapse_store_table_as_csv(
    syn,
    cells,
    "syn59424203",
    "cells"
  )

}
