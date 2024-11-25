patients_htan <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  select_columns_patients <- function(df){
    df %>%
      dplyr::select(
        "name" = "HTAN.Participant.ID",
        "age_at_diagnosis" = "Age.at.Diagnosis",
        "ethnicity" = "Ethnicity",
        "gender" = "Gender",
        "height" = "Height",
        "race" = "Race",
        "weight" = "Weight"
      )
  }

  vanderbilt_files <- c("syn38868669", #demographics
                        "syn39051142", #diagnosis
                        "syn39051632" #follow up
  )

  #For Vanderbilt
  files_path <- paste("inst/", vanderbilt_files, ".csv", sep = "") #locally stored, ideally will change to read directly from synapse
  vanderbilt_files <- purrr::map_dfr(files_path, read.csv) %>%
    dplyr::select(-c(Component, entityId, Id)) %>%
    dplyr::group_by(HTAN.Participant.ID) %>%
    dplyr::summarise(across(everything(), ~dplyr::first(na.omit(.)))) %>%
    select_columns_patients()

  vanderbilt_files$weight <-as.numeric(vanderbilt_files$weight)


  patients <-
    dplyr::bind_rows(
      msk_files,
      vanderbilt_files
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


  synapse_store_table_as_csv(
    syn,
    patients,
    "syn53678251",
    "patients"
  )

}
