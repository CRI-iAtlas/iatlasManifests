features_htan_msk <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()


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
      "Component" = "features"
    )

  synapse_store_table_as_csv(
    syn,
    features,
    "syn53701247",
    "features"
  )

}
