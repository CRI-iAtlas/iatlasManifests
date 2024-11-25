samples_htan <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  #biospecimen file
  vanderbilt <- "syn38868462"

  select_columns_samples <- function(df){
    df %>%
      dplyr::select(
        "name" = "HTAN.Biospecimen.ID",
        "patient_name" = "HTAN.Parent.ID"
      )
  }


  patients <- synapse_csv_id_to_tbl(syn, "syn53678270") %>%
    dplyr::select(
      "patient_name" = "name",
      "patient_id" = "id"
    )

  samples <- select_columns_samples(read.csv(paste("inst/",vanderbilt, ".csv", sep = "")))%>%
    dplyr::inner_join(patients, by = "patient_name") %>%
    dplyr::select(-"patient_name") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "samples"
    )

  synapse_store_table_as_csv(
    syn,
    samples,
    "syn53678285",
    "samples"
  )

}
