features_to_samples_htan_ohsu <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  #biospecimen file
  ohsu <- "syn39141309"

  samples <- synapse_csv_id_to_tbl(syn, "") %>% #UPDATE
    dplyr::select("sample_name" = "name",
                  "sample_id" = "id",
                  "age_at_diagnosis"
    )

  TIDE_df <-
    synapse_tsv_id_to_tbl(syn, "syn63542972") %>%
    dplyr::select(
      "HTAN.Biospecimen.ID" = "...1",
      "feature_value" = "TIDE"
    ) %>%
    dplyr::mutate("feature_name" = "TIDE")

  features <-
    synapse_csv_id_to_tbl(syn, "syn51613666") %>%
    dplyr::select(
      "feature_name" =  "name",
      "feature_id" = "id"
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn50944340") %>%
        dplyr::select(
          "feature_name" =  "name",
          "feature_id" = "id"
        ) #add features in TCGA table
    )

  #computing Timepoint_Relative_Order

  timepoint_order_df <-
    read.csv(paste("inst/",ohsu, ".csv", sep = "")) %>%
    dplyr::filter(HTAN.Biospecimen.ID %in% samples$sample_name) %>%
    dplyr::mutate(
      "Timepoint_Relative_Order" = dplyr::dense_rank(Collection.Days.from.Index)
    ) %>%
    dplyr::select(
      "HTAN.Biospecimen.ID",
      "Timepoint_Relative_Order"
    ) %>%
    tidyr::pivot_longer(-HTAN.Biospecimen.ID, names_to = "feature_name", values_to = "feature_value" )


  features_to_samples <- #so far, just added Timepoint_Relative_Order
    timepoint_order_df %>%
    dplyr::inner_join(features, by = "feature_name") %>%
    dplyr::inner_join(features, by = "feature_name") %>%
    dplyr::inner_join(samples, by = dplyr::join_by("HTAN.Biospecimen.ID"=="sample_name")) %>%
    dplyr::filter(!is.na(feature_value)) %>%
    dplyr::select(
      "feature_id",
      "sample_id",
      "feature_to_sample_value" = "feature_value"
    ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "features_to_samples"
    )


  synapse_store_table_as_csv(
    syn,
    features_to_samples,
    "", #update
    "features_to_samples"
  )
}



