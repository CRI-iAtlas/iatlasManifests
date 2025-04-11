patients_damrauer_rose_zappasodi <- function(){

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
    synapse_csv_id_to_tbl(syn, "syn65986772") %>%
    dplyr::mutate(
      "name" = paste("Damrauer_NatComm_2022",patient_name, sep = "-")) %>%
    dplyr::add_row(dplyr::mutate(synapse_csv_id_to_tbl(syn, "syn65986580"),
                                 "name" = paste("Rose_BrJCancer_2021",patient_name, sep = "-"))) %>%
    dplyr::add_row(dplyr::mutate(synapse_csv_id_to_tbl(syn, "syn65941750"),
                                 "name" = paste("Zappasodi_Nature_2021",patient_name, sep = "-"))) %>%
    dplyr::select(name,patient_race, patient_ethnicity, patient_gender,patient_age_at_diagnosis)  %>%
    dplyr::mutate(
      "race" = dplyr::case_when(
        patient_race == "asian_race" ~ "asian",
        patient_race == "white_race" ~ "white",
        patient_race == "black_or_african_american_race" ~ "black or african american",
        patient_race == "na_race" ~ NA_character_,
        is.na(patient_race) ~ NA_character_
      ),
      "ethnicity" = dplyr::case_when(
        patient_ethnicity == "hispanic_or_latino_ethnicity" ~ "hispanic or latino",
        patient_ethnicity == "not_hispanic_or_latino_ethnicity" ~ "not hispanic or latino",
        is.na(patient_ethnicity) ~ NA_character_
      ),
      "gender" =  tolower(patient_gender)
    ) %>%
    dplyr::select(
      "name",
      "age_at_diagnosis" = "patient_age_at_diagnosis",
      "race",
      "ethnicity",
      "gender"
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    patients,
    "syn66227460",
    "patients"
  )

}


