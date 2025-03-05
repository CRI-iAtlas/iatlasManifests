features_TEMPLATE <- function() {#UPDATE function name

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  #For most datasets, there is no need to add a features file. We only need to do this in case there is a new NUMERICAL feature that we want to record for each sample in a dataset
  #In this template, we used the code that was used to add features for single-cell RNA-seq samples: UMAP coordinates. The final table has the folowing columns:
  # name = name of the feature, with no spaces, lower cases
  # display = the name that will be displayed at the app
  # feature_class = you can assign NA, you can use a current class, or you can create a new one (in this exammple, we created the "umap" class). For the current classes in the database, run: unique((iatlasGraphQLClient::query_features())$class)
  # unit = options: NA, "Year", "Fraction", "Score", "Count", "Per Megabase"
  # method_tag = if you add a description of the method to generate this feature, add the file name (without extension) here. Otherwise, set to NA

  features <-
    dplyr::tibble("name" = character(),
                  "display" = character(),
                  "feature_class" = character(),
                  "unit" = character(),
                  "method_tag" = character()) %>%
    dplyr::add_row("name" ="umap_1",
                   "display" = "UMAP 1",
                   "feature_class" = "umap",
                   "unit" = NA_character_,
                   "method_tag" = NA_character_) %>%
    dplyr::add_row("name" ="umap_2",
                   "display" = "UMAP 2",
                   "feature_class" = "umap",
                   "unit" = NA_character_,
                   "method_tag" = NA_character_) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
    )

  synapse_store_table_as_csv(
    syn,
    features,
    "", #UPDATE
    "features"
  )

}
