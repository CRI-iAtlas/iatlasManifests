patients_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  patients <-
    synapse_feather_id_to_tbl(syn, "syn22128019") %>%
    dplyr::select(
      "name" = "ParticipantBarcode",
      "age_at_diagnosis" = "age_at_initial_pathologic_diagnosis",
      "ethnicity",
      "gender",
      "height",
      "race",
      "weight"
    ) %>%
    dplyr::mutate_at(c("ethnicity", "gender", "race"), ~tolower(.x)) %>%
    dplyr::mutate(
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


  readr::write_csv(patients, "synapse_storage_manifest.csv", na = "")

}



