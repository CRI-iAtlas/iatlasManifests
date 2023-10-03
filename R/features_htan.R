features_htan <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  features <- data.frame(
    "name"= "Timepoint_Relative_Order",
    "display" = "Relative Order of Sample Collection",
    "unit" = "Score",
    "feature_class" = "Sample metadata"
  ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "features"
    )

  # synapse_store_table_as_csv(
  #   syn,
  #   features,
  #   "", #REPLACE
  #   "features"
  # )

}
