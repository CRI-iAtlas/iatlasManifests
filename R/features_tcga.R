features_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  features <-
    synapse_feather_id_to_tbl(syn, "syn22128265") %>%
    dplyr::filter(
      .data$VariableType == "Numeric",
      !is.na(.data$FriendlyLabel)
    ) %>%
    dplyr::select(
      "name" = "FeatureMatrixLabelTSV",
      "display" = "FriendlyLabel",
      "class" = "Variable Class",
      "order" = "Variable Class Order",
      "unit" = "Unit"
    ) %>%
    dplyr::mutate(
      "name" = stringr::str_replace_all(.data$name, "[\\.]", "_"),
      class = dplyr::if_else(is.na(.data$class), "Miscellaneous", class),
      class = dplyr::if_else(.data$display %in% c("OS", "PFI"), "Survival Status", class),
      class = dplyr::if_else(.data$display %in% c("OS Time", "PFI Time"), "Survival Time", class),
      display = dplyr::if_else(
        .data$name == "age_at_initial_pathologic_diagnosis",
        "Age At Diagnosis",
        .data$display
      ),
      name = dplyr::if_else(
        .data$name == "age_at_initial_pathologic_diagnosis",
        "age_at_diagnosis",
        .data$name
      )
    ) %>%
    dplyr::add_row(
      "name" = "Tumor_fraction",
      "display" = "Tumor Fraction",
      "class" = "Overall Proportion",
      "order" = 4,
      "unit"  = "Fraction"
    ) %>%
    dplyr::add_row(
      "name" = "totTCR_reads",
      "display" = "Total TCR reads",
      "class" = "Miscellaneous"
    ) %>%
    dplyr::filter(
      !(
        .data$name == "til_percentage" &
          .data$display == "TIL Regional Fraction (Percent)" &
          class == "Overall Proportion"
      )
    ) %>%
    dplyr::arrange(.data$name) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "features"
    )

  readr::write_csv(features, "synapse_storage_manifest.csv")

}



