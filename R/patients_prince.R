patients_prince <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  patients <-
    synapse_csv_id_to_tbl(syn, "syn51252904") %>%
    dplyr::select(
      "name" = "subject.id",
      "age_at_diagnosis" = "subject.age",
      "ethnicity" = "subject.ethnicity",
      "gender" = "subject.sex",
      "race" = "subject.race",
    ) %>%
    dplyr::mutate(
      "ethnicity" = dplyr::if_else(
        "ethnicity" == "not-hispanic",
        "not_hispanic_or_latino_ethnicity",
        "hispanic_or_latino_ethnicity"
      ),
      "race" = dplyr::case_when(
        "race" == "african-american" ~ "black_or_african_american_race",
        TRUE ~ paste0(.data$race, "_race")
      )
    ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    patients,
    "syn63327058",
    "patients"
  )

}
