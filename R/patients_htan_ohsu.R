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
      "ethnicity" = dplyr::if_else(
        .data$ethnicity %in% c("not hispanic or latino", "hispanic or latino"),
        .data$ethnicity,
        NA_character_
      ),
      "race" = dplyr::if_else(
        .data$race %in% c(
          "white",
                 "black or african american",
                 "asian",
                 "native hawaiian or other pacific islander",
                 "american indian or alaska native"
        ),
        .data$race,
        NA_character_
      )
    ) %>%
    dplyr::arrange(.data$name) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "patients"
    )


  #readr::write_csv(patients, "synapse_storage_manifest.csv", na = "")

}



