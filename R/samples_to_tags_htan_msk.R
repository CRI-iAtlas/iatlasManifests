samples_to_tags_htan_msk <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  tags_htan <- synapse_csv_id_to_tbl(syn, "syn53605383") %>% #table with consolidated annotation that was created with htan_create_wide_table_vanderbilt.R
    tidyr::pivot_longer(-c("HTAN.Biospecimen.ID", "HTAN.Parent.ID"),
                        names_to = "parent_tag",
                        values_to = "tag") %>%
    dplyr::select(
      "sample" = "HTAN.Biospecimen.ID",
      "tag"
    )


  samples <-
    synapse_csv_id_to_tbl(syn, "syn53678348") %>%
    dplyr::select(
      "sample_name" = "name",
      "sample_id" = "id",
      "patient_id"
    )

  patients <- synapse_csv_id_to_tbl(syn, "syn53678262") %>%
    dplyr::select("patient_id" = "id", "ethnicity", "gender", "race") %>%
    dplyr::inner_join(samples, by = "patient_id") %>%
    dplyr::mutate(
      race = paste0(gsub(" ", "_", race), "_race"),
      ethnicity = paste0(gsub(" ", "_", ethnicity), "_ethnicity")
    ) %>%
    tidyr::pivot_longer(-c("sample_name", "patient_id", "sample_id"),
                        names_to = "parent_tag",
                        values_to = "tag") %>%
    dplyr::select(
      "sample" = "sample_name",
      "tag"
    )


  tags <-
    synapse_csv_id_to_tbl(syn, "syn53698018") %>% #msk tags
    dplyr::add_row(
    synapse_csv_id_to_tbl(syn, "syn53697423") #vanderbilt tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn51613683") #ici tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn51080176")  #add tags from TCGA
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
    rbind(tags_htan, patients) %>%
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
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    samples_to_tags,
    "syn53697617",
    "samples_to_tags"
  )

}
