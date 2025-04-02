patients_anders <- function(){ #UPDATE function name

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  # columns in the patients table:
  # name = patient name. Recommended to append a dataset identifier to the ID
  # race = options are:"white", "black or african american", "asian" "native hawaiian or other pacific islander", "american indian or alaska native", NA
  # ethnicity = options are: "hispanic or latino", "not hispanic or latino", NA
  # gender = options are: "female", "male", NA
  # age_at_diagnosis
  # weight (if available)
  # height (if available)
  # id = id created in this script for each patient

  patients <-
    synapse_csv_id_to_tbl(syn, "syn65887903") %>%
    dplyr::select(patient_name,patient_race, patient_ethnicity, patient_gender,patient_age_at_diagnosis)  %>%
    dplyr::mutate(
      "name" = patient_name,
      "race" = NA_character_,
      "ethnicity" =  NA_character_,
      "gender" =  "female"
    ) %>%
    dplyr::select(
      "name",
      "age_at_diagnosis" = "patient_age_at_diagnosis",
      "race",
      "ethnicity"
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    patients,
    "syn65888280",
    "patients"
  )

}



