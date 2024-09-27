samples_to_tags_htan <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  #get data from Synapse

  tags_htan <- synapse_csv_id_to_tbl(syn, "syn52570142") %>% #table with consolidated annotation
    tidyr::pivot_longer(-c("HTAN.Biospecimen.ID", "HTAN.Parent.ID"),
                        names_to = "parent_tag",
                        values_to = "tag") %>%
    dplyr::select(
      "sample" = "HTAN.Biospecimen.ID",
      "tag"
    )


  immune_subtypes <- synapse_csv_id_to_tbl(syn, "syn63542971") %>%
    dplyr::mutate(
      "Immune_Subtype" = paste0("C", BestCall),
      "sample_name" = gsub("\\.", "-", SampleIDs)
    ) %>%
    dplyr::select(
      "sample_name",
      "Immune_Subtype"
    ) %>%
    tidyr::pivot_longer(- "sample_name",
                        names_to = "parent_tag",
                        values_to = "tag") %>%
    dplyr::select(
      "sample_name",
      "tag"
    )

  tide_result <- synapse_tsv_id_to_tbl(syn, "syn63542972") %>%
    dplyr::mutate(
      "TIDE_Responder" = dplyr::if_else(
        Responder == FALSE,
        "false_tide_responder",
        "true_tide_responder"
      ),
      "TIDE_No_Benefits" = dplyr::if_else(
        `No benefits` == FALSE,
        "false_tide_no_benefits",
        "true_tide_no_benefits"
      )
    ) %>%
    dplyr::select(
      "sample_name" = "...1",
      "TIDE_Responder",
      "TIDE_No_Benefits"
    ) %>%
    tidyr::pivot_longer(- "sample_name",
                        names_to = "parent_tag",
                        values_to = "tag") %>%
    dplyr::select(
      "sample_name",
      "tag"
    )

  patient_tags <- synapse_csv_id_to_tbl(syn, "") %>% #REPLACE
    dplyr::rename("patient_id" = "id") %>%
    dplyr::select(-"name")

  samples <-
    synapse_csv_id_to_tbl(syn, "") %>% #REPLACE
    dplyr::inner_join(patient_tags, by = "patient_id") %>%
    dplyr::select(
      "sample_name" = "name",
      "sample_id" = "id",
      "ethnicity",
      "gender",
      "race"
    )

  tags <-
    synapse_csv_id_to_tbl(syn, "syn51613683") %>% #ici specific tags
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn51080176") #add tags from tcga
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "") #htan tags REPLACE
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
    tags_htan %>%
    dplyr::add_row(immune_subtypes) %>%
    dplyr::add_row(tide_result) %>%
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

  # synapse_store_table_as_csv(
  #   syn,
  #   samples_to_tags,
  #   "", #UPDATE
  #   "samples_to_tags"
  # )

}
