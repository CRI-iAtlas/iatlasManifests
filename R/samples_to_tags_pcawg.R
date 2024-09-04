samples_to_tags_pcawg <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  samples <-
    synapse_csv_id_to_tbl(syn, "syn51088163") %>%
    dplyr::select(
      "sample" = "name",
      "sample_id" = "id"
    )

  tags <-
    dplyr::bind_rows(
      synapse_csv_id_to_tbl(syn, "syn51080176"),
      synapse_csv_id_to_tbl(syn, "syn51088201")
    ) %>%
    dplyr::select(
      "tag" = "name",
      "tag_id" = "id"
    )

  samples_to_pcawg_study_tags <-
    synapse_tsv_id_to_tbl(syn, "syn21785582") %>%
    dplyr::select("sample" = "icgc_donor_id", "tag" = "dcc_project_code")

  samples_to_pcawg_study <- samples_to_pcawg_study_tags  %>%
    dplyr::mutate(tag = "PCAWG_Study")

  samples_to_immune_subtype_tags <-
    synapse_tsv_id_to_tbl(syn, "syn20717211" ) %>%
    dplyr::select("sample", "tag" = "subtype")

  samples_to_immune_subtype <- samples_to_immune_subtype_tags %>%
    dplyr::mutate(tag = "Immune_Subtype")

  samples_to_gender_tags <-
    synapse_csv_id_to_tbl(syn, "syn51088152") %>%
    dplyr::select("sample" = "name", "tag" = "gender")

  samples_to_gender <- samples_to_gender_tags %>%
    dplyr::mutate(tag = "gender")

  samples_to_tags <-
    dplyr::bind_rows(
      samples_to_pcawg_study_tags,
      samples_to_immune_subtype_tags,
      samples_to_gender_tags,
      samples_to_pcawg_study,
      samples_to_immune_subtype,
      samples_to_gender
    ) %>%
    dplyr::inner_join(tags, by = "tag") %>%
    dplyr::inner_join(samples, by = "sample") %>%
    dplyr::select(-c("sample", "tag")) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    samples_to_tags,
    "syn51092812",
    "samples_to_tags"
  )

}
