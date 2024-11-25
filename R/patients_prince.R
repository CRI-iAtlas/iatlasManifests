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
        ethnicity == "not-hispanic",
        "not hispanic or latino",
        "hispanic or latino"
      ),
      "race" = dplyr::case_when(
        race == "african-american"  ~ "black or african american",
        race == "white"  ~ "white",
        race == "asian"  ~ "asian",
        race == "other"  ~ NA_character_,
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
