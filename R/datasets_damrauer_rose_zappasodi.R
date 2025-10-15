dataset_damrauer_rose_zappasodi <- function(){ #UPDATE function name

  require(magrittr)
  require(rlang)


  syn <- create_synapse_login()
  #add a "name", with no spaces, to the dataset
  #add a "display", which is the name that will be displayed at the app. For datasets with treatment info, we usually add the tumor type and treatment info
  #add "dataset_type", options are "cg" for cancer genomics, "ici" for immmune checkpoint inhibitor datasets, and "scrna" for single-cell RNA seq dataset
  datasets <-
    data.frame("name" = c("Damrauer_NatComm_2022", "Rose_BrJCancer_2021", "Zappasodi_Nature_2021"), #UPDATE
               "display" = c("Damrauer 2022 - BLCA, PD-L1", "Rose 2021 - BLCA, PD-1/PD-L1","Zappasodi 2021 - SKCM, CTLA4")) %>%
    dplyr::mutate(
      "dataset_type" = "ici",
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )


  synapse_store_table_as_csv(
    syn,
    datasets,
    "syn66227459", #UPDATE with synapse ID for folder named "Dataset [NAME OF DATASET]"
    "datasets"
  )

}
