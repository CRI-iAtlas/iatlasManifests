tags_to_tags_tcga <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  tags <-
    synapse_csv_id_to_tbl(syn, "syn51080176") %>%
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )

  new_tags_to_tags <- data.frame(
    "tag" = c("female", "male",
              "white_race", "black_or_african_american_race", "asian_race", "native_hawaiian_or_other_pacific_islander_race", "american_indian_or_alaska_native_race",
              "na_ethnicity", "not_hispanic_or_latino_ethnicity", "hispanic_or_latino_ethnicity"),
    "related_tag" = c("gender", "gender",
                      "race", "race", "race", "race", "race",
                      "ethnicity", "ethnicity", "ethnicity")
  )

  tags_to_tags <-
    synapse_feather_id_to_tbl(syn, "syn23545186") %>%
    dplyr::bind_rows(new_tags_to_tags) %>%
    dplyr::select("tag_name" = "tag", "related_tag_name" = "related_tag") %>%
    dplyr::inner_join(tags, by = c("related_tag_name" = "tag_name")) %>%
    dplyr::rename("related_tag_id" = "tag_id") %>%
    dplyr::select(-"related_tag_name") %>%
    dplyr::inner_join(tags, by = "tag_name") %>%
    dplyr::select(-"tag_name") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "tags_to_tags"
    )


  synapse_store_table_as_csv(
    syn,
    tags_to_tags,
    "syn51080195",
    "tags_to_tags"
  )

}
