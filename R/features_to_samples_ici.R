samples_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  features <-
    synapse_csv_id_to_tbl(syn, "syn51613666") %>%
    dplyr::select(
      "feature_name" =  "name",
      "feature_id" = "id"
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn50944340") %>%
        dplyr::filter(.data$feature_class != "Clinical") %>%
        dplyr::select(
          "feature_name" =  "name",
          "feature_id" = "id"
        ) #add features in TCGA table
    )

  samples <-
    synapse_csv_id_to_tbl(syn, "syn51589463") %>%
    dplyr::select(
      "sample_name" = "name",
      "sample_id" = "id"
    )

  features_to_samples <-
    synapse_feather_id_to_tbl(syn, "syn26033314") %>%  #cibersort #original
    #dplyr::add_row(synapse_feather_id_to_tbl(syn, "syn26033315")) %>% #epic #original
    #dplyr::add_row(synapse_feather_id_to_tbl(syn, "syn26033316")) %>% #mcpcounter #original
    dplyr::add_row(synapse_feather_id_to_tbl(syn, "syn25981855")) %>% #features to samples #replace
    dplyr::add_row(synapse_feather_id_to_tbl(syn, "syn27790752")) %>% #features to samples nanostring #replace
    dplyr::filter(feature %in% features$feature_name) %>%
    dplyr::inner_join(features, by = dplyr::join_by("feature"=="feature_name")) %>%
    dplyr::inner_join(samples, by = dplyr::join_by("sample"=="sample_name")) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::select(
      "feature_id",
      "sample_id",
      "feature_to_sample_value" = "value"
    ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "features_to_samples"
    )


  synapse_store_table_as_csv(
    syn,
    features_to_samples,
    "syn52069095",
    "features_to_samples"
  )
}



