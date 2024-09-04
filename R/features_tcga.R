features_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  methods <-
    synapse_feather_id_to_tbl(syn, "syn22130608") %>%
    dplyr::select("origin" = "Feature Origin", "method_tag" = "Methods Tag") %>%
    tidyr::drop_na()

  germline_features <-
    synapse_feather_id_to_tbl(syn, "syn25170093")

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
      "unit" = "Unit",
      "origin" = "Origin"
    ) %>%
    dplyr::left_join(methods, by = "origin") %>%
    dplyr::select(-"origin") %>%
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
      ),
      method_tag = dplyr::if_else(
        .data$name == "TCR_Evenness",
        "TCR",
        .data$method_tag
      )
    ) %>%
    #dplyr::filter(!is.na(.data$method_tag)) %>% #commenting it out because this filter removes all TIL Maps features and age, weight, height
    dplyr::add_row(
      "name" = "Tumor_fraction",
      "display" = "Tumor Fraction",
      "class" = "Overall Proportion",
      "order" = 4,
      "unit"  = "Fraction",
      "method_tag" = "LF"
    ) %>%
    dplyr::add_row(
      "name" = "totTCR_reads",
      "display" = "Total TCR reads",
      "class" = "Miscellaneous",
      "method_tag" = "TCR"
    ) %>%
    dplyr::filter(
      !(
        .data$name == "til_percentage" &
          .data$display == "TIL Regional Fraction (Percent)" &
          class == "Overall Proportion"
      )
    ) %>%
    dplyr::arrange(.data$name) %>%
    dplyr::rename("feature_class" = "class") %>%
    dplyr::left_join(germline_features, by = "name") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "features"
    )

  synapse_store_table_as_csv(
    syn,
    features,
    "syn50944339",
    "features"
  )

}



