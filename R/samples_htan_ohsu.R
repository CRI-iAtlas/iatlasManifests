samples_htan <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  #biospecimen file
  ohsu <- "syn39141309"

  #samples ids to be kept
  samples_ids <- c(
    "HTA9_1_1",
    "HTA9_1_9",
    "HTA9_1_33",
    "HTA9_1_86",
    "HTA9_1_97"
  )


  patients <- synapse_csv_id_to_tbl(syn, "syn63600274") %>%
    dplyr::select(
      "patient_name" = "name",
      "patient_id" = "id"
    )

  samples <- read.csv(paste("inst/",ohsu, ".csv", sep = "")) %>%  #should we select the 5 assayed samples?
    dplyr::filter(HTAN.Biospecimen.ID %in% samples_ids) %>%
    dplyr::mutate(HTAN.Parent.ID = replace(HTAN.Parent.ID, HTAN.Parent.ID == "HTA9_1_6", "HTA9_1")) %>% #changing patient ID to version in Therapy
    dplyr::select(
      "name" = "HTAN.Biospecimen.ID",
      "patient_name" = "HTAN.Parent.ID"
    ) %>%
    dplyr::inner_join(patients, by = "patient_name") %>%
    dplyr::select(-"patient_name") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

    synapse_store_table_as_csv(
      syn,
      samples,
      "syn63600264",#update with folder id
      "samples"
    )

}



