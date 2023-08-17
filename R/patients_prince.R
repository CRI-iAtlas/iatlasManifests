patients_prince <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  patients_prince <-
    synapse_csv_id_to_tbl(syn, "syn51252904") %>%
    dplyr::select(
      "name" = "subject.id",
      "age_at_diagnosis" = "subject.age",
      "ethnicity" = "subject.ethnicity",
      "gender" = "subject.sex",
      "race" = "subject.race",
    ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "patients"
    )

  # synapse_store_table_as_csv(
  #   syn,
  #   patients,
  #   "update",
  #   "patients"
  # )

}
