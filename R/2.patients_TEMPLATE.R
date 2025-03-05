patients_TEMPLATE <- function(){ #UPDATE function name

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


  #below is an example of how the AMADEUS patient table was generated. You'll need to update it with the references to get the necessary data
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
      "name" = paste0("AMADEUS_", `Subject ID`), #here we are appending the dataset name to the patient name
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
    "", #UPDATE with synapse ID for folder named "Patients [NAME OF DATASET]"
    "patients"
  )

}



