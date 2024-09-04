patients_shiao <- function(){

  require(magrittr)
  require(rlang)
  require(openxlsx)
  syn <- create_synapse_login()

  #the only info on patient level for this dataset is age, stored in a supplemental table

  age_info <-
    syn$get("syn58403343") %>%
    purrr::pluck("path") %>%
    openxlsx::read.xlsx(., sheet = 2) %>%
    dplyr::as_tibble()


  patients <- age_info %>%
    dplyr::select(
      "Paper.Number",
      "age_at_diagnosis" = "Age"
    ) %>%
    dplyr::mutate(
      "name" = dplyr::if_else( #we need to format the patient ID to match the ID in the h5ad, and also add study identifier
        nchar(.$Paper.Number) == 1,
        paste0("Shiao_BRCA_Patient0", Paper.Number),
        paste0("Shiao_BRCA_Patient", Paper.Number),
      ),
      "gender" = "female",
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
    "syn58399499",
    "patients"
  )

}
