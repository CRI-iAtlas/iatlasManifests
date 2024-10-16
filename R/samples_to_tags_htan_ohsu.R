samples_to_tags_htan_ohsu <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  #get data from Synapse

  tags_htan <- synapse_csv_id_to_tbl(syn, "syn52570142") %>% #table with consolidated annotation
    tidyr::pivot_longer(-c("HTAN.Biospecimen.ID", "HTAN.Parent.ID"),
                        names_to = "parent_tag",
                        values_to = "tag") %>%
    dplyr::select(
      "sample_name" = "HTAN.Biospecimen.ID",
      "tag"
    )


  immune_subtypes <- synapse_csv_id_to_tbl(syn, "syn63542971") %>%
    dplyr::mutate(
      "Immune_Subtype" = paste0("C", BestCall),
      "sample_name" =  substr(SampleIDs, 26, 40)
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
      "sample_name" =  substr(`...1`, 26, 40),
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
      "sample_name",
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

  patient_tags <- synapse_csv_id_to_tbl(syn, "syn63600274") %>%
    dplyr::mutate(
    "ethnicity" = dplyr::case_when(
      .data$ethnicity == "not hispanic or latino" ~ "not_hispanic_or_latino_ethnicity",
      .data$ethnicity == "hispanic or latino" ~ "hispanic_or_latino_ethnicity",
      is.na(.data$ethnicity) ~ NA_character_
    ),
    "race" = "white_race"
    )
    dplyr::rename("patient_id" = "id") %>%
    dplyr::select(-"name")

  samples <-
    synapse_csv_id_to_tbl(syn, "syn63600384") %>%
    dplyr::inner_join(patient_tags, by = "patient_id") %>%
    dplyr::select(
      "sample_name" = "name",
      "sample_id" = "id",
      "ethnicity",
      "gender",
      "race"
    ) %>%
    tidyr::pivot_longer(- c("sample_name", "sample_id"),
                        names_to = "parent_tag",
                        values_to = "tag") %>%
    dplyr::select(
      "sample_name",
      "sample_id",
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
      synapse_csv_id_to_tbl(syn, "syn63389543") #PRINCE specific
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn63600708")
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
    dplyr::add_row(dplyr::select(samples, sample_name, tag)) %>%
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
    dplyr::inner_join(dplyr::distinct(dplyr::select(samples, sample_name, sample_id)), by = "sample_name") %>%
    dplyr::select(-c("sample_name", "tag_name")) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    samples_to_tags,
    "syn63600269",
    "samples_to_tags"
  )

}
