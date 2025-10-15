samples_anders <- function(){ #UPDATE function name

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  # columns in the SAMPLES table:
  # name = sample name. Recommended to append a dataset identifier to the ID
  # patient_id = id for the associated patient, generated in the patients table
  # id = id created in this script for each sample

  patients <- #get the patient names and ids
    synapse_csv_id_to_tbl(syn, "syn65900260") %>% #update with synapse ID for the patient file
    dplyr::select(
      "patient_name" = "name",
      "patient_id" = "id"
    )


  samples <-
    synapse_csv_id_to_tbl(syn, "syn65887903") %>% #manifest file, to get patient - sample relationship
    dplyr::select(
      "name" = "sample_name",
      "patient_name"
    ) %>%
    dplyr::inner_join(patients, by = "patient_name") %>%
    dplyr::select("name", "patient_id") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )


  synapse_store_table_as_csv(
    syn,
    samples,
    "syn65888281",
    "samples"
  )

}



