samples_prince <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  source("patients_prince.R")
  patients <-
    patients_prince() %>% #replace with
    dplyr::select(
      "patient_name" = "name",
      "patient_id" = "id"
    )

  samples <-
      synapse_csv_id_to_tbl(syn, "syn51252936") %>%
    dplyr::select(
      "name" = "sample.id",
      "patient_name" = "subject.id"
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
  #   update!,
  #   "samples"
  # )

}
