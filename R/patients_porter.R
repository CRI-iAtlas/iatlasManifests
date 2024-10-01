patients_ici <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()


  patients <-
    #clinical_data %>%
    synapse_csv_id_to_tbl(syn, "syn54031467") %>%
    dplyr::mutate(
      "race" = dplyr::case_when(
        Race == "Asian" ~ "asian_race",
        Race == "White" ~ "white_race",
        Race == "Other" ~ "other_race",
        Race == "Black or African American" ~ "black_or_african_american_race",
      ),
      "ethnicity" = dplyr::case_when(
        Ethnicity == "Hispanic or Latino" ~ "hispanic_or_latino_ethnicity",
        Ethnicity == "Not Hispanic or Latino" ~ "not_hispanic_or_latino_ethnicity",
      ),
      "gender" = "male"
    ) %>%
    dplyr::select(
      "name" = "Subject ID",
      "age_at_diagnosis" = "Age",
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
    #"",
    "patients"
  )

}



