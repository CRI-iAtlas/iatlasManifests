dataset_TEMPLATE <- function(){ #UPDATE function name

  require(magrittr)
  require(rlang)


  syn <- create_synapse_login()
  #add a "name", with no spaces, to the dataset
  #add a "display", which is the name that will be displayed at the app. For datasets with treatment info, we usually add the tumor type and treatment info
  #add "dataset_type", options are "cg" for cancer genomics, "ici" for immmune checkpoint inhibitor datasets, and "scrna" for single-cell RNA seq dataset
  datasets <-
    data.frame("name" = c(""), #UPDATE
               "display" = c("AMADEUS - several tumor types, PD-1 +/- CTLA4"), #UPDATE
               "dataset_type" = "") %>% #UPDATE
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )


  synapse_store_table_as_csv(
    syn,
    datasets,
    "", #UPDATE with synapse ID for folder named "Dataset [NAME OF DATASET]"
    "datasets"
  )

}

