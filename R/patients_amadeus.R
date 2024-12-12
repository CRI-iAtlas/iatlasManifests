patients_amadeus <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  #not all patients in the clinical data table have RNA seq data, so we'll only store data for those who are in the manifest
  patients_manifest <- synapse_tsv_id_to_tbl(syn, "syn64290150") %>%
    dplyr::bind_rows(synapse_tsv_id_to_tbl(syn, "syn64369373") ) %>%
    dplyr::mutate(
      patient = dplyr::case_when(
        Patient_Name == "104-0013/104-0022" ~ "104-0022",
        TRUE ~ sub("_.*$", "",  Patient_Name)
      )
    )


  patients <-
    synapse_csv_id_to_tbl(syn, "syn54074560") %>%
    dplyr::filter(`Subject ID` %in% patients_manifest$patient) %>%
    dplyr::mutate(
      "name" = paste0("AMADEUS_", `Subject ID`),
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
      "gender" =  dplyr::case_when(
        Sex == "F" ~ "female",
        Sex == "M" ~ "male"
      )
    ) %>%
    dplyr::select(
      "name",
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
    "syn64156729",
    "patients"
  )

}



