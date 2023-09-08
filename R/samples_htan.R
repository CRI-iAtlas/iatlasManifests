samples_htan <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  #biospecimen file
  ohsu <- "syn39141309"

  patients <- patients_htan() %>%  #update to read from patients in synapse id for patients_htan
    dplyr::select(
      "patient_name" = "name",
      "patient_id" = "id"
    )

  samples <- read.csv(paste("inst/",ohsu, ".csv", sep = "")) %>%  #should we select the 5 assayed samples?
    dplyr::select(
      "name" = "HTAN.Biospecimen.ID",
      "patient_name" = "HTAN.Parent.ID"
    ) %>%
    dplyr::inner_join(patients, by = "patient_name") %>%
    dplyr::select(-"patient_name") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "samples"
    )

  # synapse_store_table_as_csv(
  #   syn,
  #   samples,
  #   "",#update with folder id
  #   "samples"
  # )

}



