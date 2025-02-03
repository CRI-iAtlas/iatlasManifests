dataset_amadeus <- function(){

  require(magrittr)
  require(rlang)


  syn <- create_synapse_login()

  datasets <-
    data.frame("name" = c("AMADEUS"), "display" = c("AMADEUS - several tumor types, PD-1 +/- CTLA4"), "dataset_type" = "ici") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )


  synapse_store_table_as_csv(
    syn,
    datasets,
    "syn64156728",
    "datasets"
  )

}

