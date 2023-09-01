samples_to_tags_prince <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  #get data from Synapse

  tags_prince <- synapse_csv_id_to_tbl(syn, "syn52349216") %>% #table with consolidated prince annotation that was created with prince_tags_function.R
    tidyr::pivot_longer(-c("sample.id", "subject.id", "timepoint.relative.order"),
                        names_to = "parent_tag",
                        values_to = "tag") %>%
    dplyr::select(
      "sample" = "sample.id",
      "tag"
    )

  samples <-
    samples_prince() %>% #REPLACE PRINCE
    dplyr::select(
      "sample_name" = "name",
      "sample_id" = "id"
    )

  tags <-
    synapse_csv_id_to_tbl(syn, "syn51613683") %>% #ici specific tags
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn51080176") #add tags from tcga
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn52371704") #PRINCE specific - REPLACE
    ) %>%
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )

  tag_names <- #keep this from samples_to_tags_tcga
    synapse_feather_id_to_tbl(syn, "syn23545011" ) %>%
    dplyr::select("tag" = "old_name", "new_tag" = "name") %>%
    tidyr::drop_na()


  samples_to_tags <-
    tags_prince %>%
    dplyr::left_join(tag_names, by = "tag", relationship = "many-to-many") %>%
    dplyr::mutate(
      "new_tag" = dplyr::if_else(
        is.na(.data$new_tag),
        .data$tag,
        .data$new_tag
      )
    ) %>%
    dplyr::select(-"tag") %>%
    dplyr::rename("tag_name" = "new_tag", "sample_name" = "sample") %>%
    dplyr::inner_join(tags, by = "tag_name") %>%
    dplyr::inner_join(samples, by = "sample_name") %>%
    dplyr::select(-c("sample_name", "tag_name")) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "samples_to_tags"
    )

  synapse_store_table_as_csv(
    syn,
    samples_to_tags,
    "", #REPLACE PRINCE
    "samples_to_tags"
  )

}
