samples_to_tags_prince <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  #get data from Synapse


  patient_tags <- synapse_csv_id_to_tbl(syn, "syn63331646") %>%
    dplyr::rename("patient_id" = "id") %>%
    dplyr::select(-"name")

  samples <-
    synapse_csv_id_to_tbl(syn, "syn63332022") %>%
    dplyr::inner_join(patient_tags, by = "patient_id") %>%
    dplyr::select(
      "sample_name" = "name",
      "sample_id" = "id",
      "ethnicity",
      "gender",
      "race"
    )

  immune_subtypes <- synapse_csv_id_to_tbl(syn, "syn63194039") %>%
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

  tide_result <- synapse_tsv_id_to_tbl(syn, "syn63296609") %>%
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

  tags_prince <- synapse_csv_id_to_tbl(syn, "syn52349216") %>%
    dplyr::select(
      -c("ph", "sample_key", "sample.id", "subject.id", "timepoint.relative.order")
    ) %>%
    dplyr::inner_join(samples, by = dplyr::join_by("Run_ID" == "sample_name")) %>%
    tidyr::pivot_longer(-c("sample_id", "Run_ID"),
                        names_to = "parent_tag",
                        values_to = "tag") %>%
    dplyr::select(
      "sample_name" = "Run_ID",
      "tag"
    )

  tags <-
    synapse_csv_id_to_tbl(syn, "syn51613683") %>% #ici specific tags
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn51080176") #add tags from tcga
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn53698018") #msk tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn60157438") #li tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn58896103") #shiao tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn59210643") #krishna tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn53697423") #vanderbilt tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn63389543") #PRINCE specific - REPLACE
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
    dplyr::rename("tag_name" = "new_tag") %>%
    dplyr::inner_join(tags, by = "tag_name") %>%
    dplyr::select(-"tag_name") %>%
    dplyr::inner_join(samples, by = "sample_name") %>%
    dplyr::select("sample_id", "tag_id") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    samples_to_tags,
    "syn63327064",
    "samples_to_tags"
  )

}
