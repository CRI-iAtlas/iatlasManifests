patients_porter <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()


  patients <-
    synapse_csv_id_to_tbl(syn, "syn54031467") %>%
    dplyr::mutate(
      "race" = dplyr::case_when(
        Race == "Asian" ~ "asian",
        Race == "White" ~ "white",
        Race == "Other" ~ NA_character_,
        Race == "Black or African American" ~ "black or african american",
      ),
      "ethnicity" = dplyr::case_when(
        Ethnicity == "Hispanic or Latino" ~ "hispanic or latino",
        Ethnicity == "Not Hispanic or Latino" ~ "not hispanic or latino",
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
    "syn63623050",
    "patients"
  )

}



