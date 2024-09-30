patients_htan <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  ohsu_files <- c("syn39141967", #demographics
                  "syn39141331", #diagnosis
                  "syn39141359" #follow up
  )

  files_path <- paste("inst/", ohsu_files, ".csv", sep = "") #locally stored, ideally will change to read directly from synapse
  ohsu_files <- purrr::map_dfr(files_path, read.csv) %>%
    dplyr::select(-c(Component, entityId, Id)) %>%
    dplyr::group_by(HTAN.Participant.ID) %>%
    dplyr::summarise(across(everything(), ~dplyr::first(na.omit(.))))



  patients <-
    ohsu_files %>%
    dplyr::filter(HTAN.Participant.ID == "HTA9_1") %>%
    dplyr::select(
      "name" = "HTAN.Participant.ID",
      "age_at_diagnosis" = "Age.at.Diagnosis",
      "ethnicity" = "Ethnicity",
      "gender" = "Gender",
      "height" = "Height",
      "race" = "Race",
      "weight" = "Weight"
    ) %>%
    dplyr::mutate_at(c("ethnicity", "gender", "race"), ~tolower(.x)) %>%
    dplyr::mutate(
      "age_at_diagnosis" = floor(.data$age_at_diagnosis/365),
      "gender" = dplyr::if_else(
        .data$gender %in% c("female", "male"),
        .data$gender,
        NA_character_
      ),
      "ethnicity" = dplyr::case_when(
        .data$ethnicity == "not hispanic or latino" ~ "not_hispanic_or_latino_ethnicity",
        .data$ethnicity == "hispanic or latino" ~ "hispanic_or_latino_ethnicity",
        is.na(.data$ethnicity) ~ NA_character_
      ),
      "race" = "white_race"
    ) %>%
    dplyr::arrange(.data$name) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )


  synapse_store_table_as_csv(
    syn,
    patients,
    "syn63600263",
    "patients"
  )

}



