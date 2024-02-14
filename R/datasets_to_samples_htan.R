datasets_to_samples_htan <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  dataset_ids <- dataset_htan() #update

  #biospecimen files
  files <-  setNames(c("syn39256250", "syn38868462"),
                     c("HTAN_MSK", "HTAN_Vanderbilt"))


  combine_samples_dataset_id <- function(dataset_name){
    dataset_id <- dataset_ids %>%
      dplyr::filter(name == dataset_name) %>%
      dplyr::pull(id)

    read.csv(paste("inst/", files[dataset_name], ".csv", sep = "")) %>%
      dplyr::select(
        "name" = "HTAN.Biospecimen.ID",
      ) %>%
    dplyr::mutate(
      "dataset_id" = dataset_id
    )
  }


  samples <- samples_htan() %>% #update
    dplyr::select(
      "name",
      "sample_id" = "id"
    )

  datasets_to_samples <- combine_samples_dataset_id("HTAN_MSK") %>%
    dplyr::bind_rows(combine_samples_dataset_id("HTAN_Vanderbilt")) %>%
    dplyr::inner_join(samples, by = "name") %>%
    dplyr::select(-name) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "datasets_to_samples"
    )

  # synapse_store_table_as_csv(
  #   syn,
  #   datasets_to_samples,
  #   "", #update
  #   "datasets_to_samples"
  # )

}
