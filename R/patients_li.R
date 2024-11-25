patients_li <- function(){

  require(magrittr)
  require(rlang)
  require(openxlsx)
  syn <- create_synapse_login()

  patient_info <-
    syn$get("syn59942707") %>%
    purrr::pluck("path") %>%
    openxlsx::read.xlsx(., sheet = 1) %>%
    dplyr::as_tibble()

  colnames(patient_info) <- patient_info[1,]
  patient_info <- patient_info[-c(1,14:23),]

  patients <- patient_info %>%
    dplyr::select(
      "Patient_ID",
      "gender" = "sex"
    ) %>%
    dplyr::mutate(
      "name" = paste0("Li_ccRCC_", Patient_ID)
    ) %>%
    dplyr::select(name, gender) %>%
    dplyr::arrange(.data$name) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "patients"
    )


  synapse_store_table_as_csv(
    syn,
    patients,
    "syn60085128",
    "patients"
  )

}



