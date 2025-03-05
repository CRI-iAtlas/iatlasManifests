samples_TEMPLATE <- function(){ #UPDATE function name

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  # columns in the SAMPLES table:
  # name = sample name. Recommended to append a dataset identifier to the ID
  # patient_id = id for the associated patient, generated in the patients table
  # id = id created in this script for each sample

  patients <- #get the patient names and ids
    synapse_csv_id_to_tbl(syn, "") %>% #update with synapse ID for the patient file
    dplyr::select(
      "patient_name" = "name",
      "patient_id" = "id"
    )

  #Below are the steps thatt were necessary to get the patient - sample relationship for the AMADEUS dataset. You might need to do something different
  samples_features <- synapse_csv_id_to_tbl(syn, "syn64154238")

  samples <-
    synapse_tsv_id_to_tbl(syn, "syn64290150") %>% #manifest file, to get patient - sample relationship
    dplyr::bind_rows(synapse_tsv_id_to_tbl(syn, "syn64369373") ) %>%
    dplyr::mutate(
      "name" = gsub("/", "_", paste(Dataset, Patient_Name, Run_Name, sep= "-")),
      "patient_name" = paste0("AMADEUS_", #here we are appending the dataset name to the patient name, as we did in the patient table
                              ifelse(Patient_Name == "104-0013/104-0022",
                                     "104-0022",
                                     sub("_.*$", "",  Patient_Name)))
    ) %>%
    dplyr::select(
      "name",
      "patient_name"
    ) %>%
    dplyr::inner_join(samples_features, by = dplyr::join_by("name" == "Run_ID")) %>% #get the ids of the samples processed by LENS
    dplyr::inner_join(patients, by = "patient_name") %>%
    dplyr::select("name", "patient_id") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )


  synapse_store_table_as_csv(
    syn,
    samples,
    "", #UPDATE
    "samples"
  )

}



