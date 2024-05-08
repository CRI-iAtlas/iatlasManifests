samples_krishna <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()


  patients_info <-
    syn$get("syn59202673") %>%
    purrr::pluck("path") %>%
    openxlsx::read.xlsx(., sheet = 1) %>%
    dplyr::as_tibble()

  patients <- synapse_csv_id_to_tbl(syn, "syn59203847") %>%
    dplyr::select(
      "patient_name" = "name",
      "patient_id" = "id"
    )


  samples <- patients_info %>%
    dplyr::select(sample_name,
                  patient_name) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      name = paste0("Krishna_ccRCC_", sample_name),
      patient_name = paste0("Krishna_ccRCC_", patient_name)
    ) %>%
    dplyr::inner_join(patients, by = "patient_name") %>%
    dplyr::select(
      "name",
      "patient_id"
    ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "samples"
    )

  synapse_store_table_as_csv(
    syn,
    samples,
    "syn59195635",
    "samples"
  )

}
