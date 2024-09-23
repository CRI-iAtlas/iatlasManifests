dataset_porter <- function(){

  require(magrittr)
  require(rlang)


  syn <- create_synapse_login()

  ici_datasets <-
    data.frame("name" = c("PORTER"), "display" = c("PORTER - mCRPC, PD-1"), "dataset_type" = "ici") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )


  synapse_store_table_as_csv(
    syn,
    datasets,
    #"syn60085087",
    "datasets"
  )

}

