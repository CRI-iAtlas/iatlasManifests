patients_krishna <- function(){

  require(magrittr)
  require(rlang)
  require(openxlsx)
  syn <- create_synapse_login()

  #the only info on patient level for this dataset is age

  patients_info <-
    syn$get("syn59202673") %>%
    purrr::pluck("path") %>%
    openxlsx::read.xlsx(., sheet = 1) %>%
    dplyr::as_tibble()


  patients <- patients_info %>%
    dplyr::select(
      "patient_name",
      "age_at_diagnosis" = "Age",
      "gender"
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      "name" = paste0("Krishna_ccRCC_", patient_name)
    ) %>%
    dplyr::select(name, age_at_diagnosis, gender) %>%
    dplyr::arrange(.data$name) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "patients"
    )


  synapse_store_table_as_csv(
    syn,
    patients,
    "syn59195594",
    "patients"
  )

}
