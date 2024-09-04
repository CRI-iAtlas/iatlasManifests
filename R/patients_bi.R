patients_bi <- function(){

  require(magrittr)
  require(rlang)
  require(openxlsx)
  syn <- create_synapse_login()

  #age and gender information are stored in a supplemental table

  patient_info <-
    syn$get("syn60531525") %>%
    purrr::pluck("path") %>%
    openxlsx::read.xlsx(., sheet = 2) %>%
    dplyr::as_tibble()

  colnames(patient_info) <- patient_info[1,]
  patient_info <- patient_info[-1,]


  patients <- patient_info %>%
    dplyr::select(
      "Sample",
      "age_at_diagnosis" = "Age at Dx",
      "Sex"
    ) %>%
    dplyr::mutate(
      "name" = paste0("Bi_ccRCC_", Sample),
      "gender" = dplyr::if_else(
        Sex == "M",
        "male",
        "female"
      ),
      "age_at_diagnosis" = trunc(as.numeric(age_at_diagnosis))
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
    "syn60530037",
    "patients"
  )

}
