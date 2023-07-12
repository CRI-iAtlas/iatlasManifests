samples_to_tags_tcga <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  samples <-
    synapse_csv_id_to_tbl(syn, "syn50896891") %>%
    dplyr::select(
      "sample_name" = "name",
      "sample_id" = "id"
    )

  tags <-
    synapse_csv_id_to_tbl(syn, "syn51080176") %>%
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )

  tag_names <-
    synapse_feather_id_to_tbl(syn, "syn23545011" ) %>%
    dplyr::select("tag" = "old_name", "new_tag" = "name") %>%
    tidyr::drop_na()


  samples_to_tags <-
    synapse_feather_id_to_tbl(syn, "syn22128019") %>%
    dplyr::select(
      "sample" = "ParticipantBarcode",
      "TCGA_Study" = "Study",
      "Immune_Subtype" = "Subtype_Immune_Model_Based",
      "TCGA_Subtype" = "Subtype_Curated_Malta_Noushmehr_et_al"
    ) %>%
    tidyr::pivot_longer(-"sample", values_to = "tag") %>%
    tidyr::drop_na() %>%
    tidyr::pivot_longer(-"sample", values_to = "tag") %>%
    dplyr::select("sample", "tag") %>%
    dplyr::left_join(tag_names, by = "tag") %>%
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
    "syn51080205",
    "samples_to_tags"
  )

}
