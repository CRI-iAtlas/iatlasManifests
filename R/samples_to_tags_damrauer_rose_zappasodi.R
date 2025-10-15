samples_to_tags_damrauer_rose_zappasodi <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  # We use the terminology "tags" to refer to categorical clinical data stored - any clinical data that is numeric should be added to the features_to_samples table
  # To illustrate the concept, consider a hypothetical example where we want to record an "Attribute_A" to samples,
  # available as a table below:

  # Sample_Name | Attribute_A
  # S_123456789 | A1

  # For this example, we would need to do the following steps:
  #1. Add "Attribute_A" to the tags table as a "parent_group"
  #2. Add "A1" to the tags table as a "group"
  #3. Add the relationship S_123456789 - A1 to the samples_to_tags table (here)
  #4. Add the relationship Attribute_A - A1 (ie, that A1 is a group under Attribute_A) to the tags_to_tags table

  # Step 3 is always required if we want to store categorical information for the samples
  # If a new dataset has no new "parent_group", step 1 is not necessary
  # If a a new dataset has no new "parent_group" and no new "group", step 1, 2 and 4 are not necessary

  # In this script, we will gather all clinical information for the dataset, format values to the patterns in "tag_name" in the tags table, and then store the sample id - tag id relationship
  # The samples_to_tags table has the following columns:
  # "tag_id" = id of the tags, should be in a tag table
  # "sample_id" = id of the sample
  # "id" = id of the relationship


  clinical_df <- synapse_csv_id_to_tbl(syn, "syn65986772") %>% #update with source of clinical info
    dplyr::bind_rows(synapse_csv_id_to_tbl(syn, "syn65986580")) %>%
    dplyr::bind_rows(synapse_csv_id_to_tbl(syn, "syn65941750")) %>%
    dplyr::mutate(
      patient_ethnicity = dplyr::if_else(
        is.na(patient_ethnicity),
        "na_ethnicity",
        patient_ethnicity
      ),
      patient_race = dplyr::if_else(
        is.na(patient_race),
        "na_race",
        patient_race
      ),
      patient_gender = dplyr::if_else(
        is.na(patient_gender),
        "na_gender",
        tolower(patient_gender)
      )
    )

  samples <-
    synapse_csv_id_to_tbl(syn, "syn66227595") %>%
    dplyr::rename(
      "sample_name" = "name",
      "sample_id" = "id"
    )

  tags <- #keep this code, and add a "add_row"statement with synapse id in case new tags were added for this dataset
    synapse_csv_id_to_tbl(syn, "syn51613683") %>% #ici specific tags
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn51080176") #add tags from tcga
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn53698018") #msk tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn60157438") #li tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn58896103") #shiao tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn59210643") #krishna tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn53697423") #vanderbilt tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn63389543") #PRINCE specific
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn63623105") #PORTER specific
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn64423867") #AMADEUS specific
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn68143413") #Damrauer, Rose specific
    ) %>%
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )

  tag_names <- #keep this from samples_to_tags_tcga
    synapse_feather_id_to_tbl(syn, "syn23545011" ) %>%
    dplyr::select("tag" = "old_name", "new_tag" = "name") %>%
    tidyr::drop_na()

  immune_subtypes <- synapse_csv_id_to_tbl(syn, "syn68143679") %>%
    dplyr::mutate(
      "Immune_Subtype" = paste0("C", BestCall),
      "sample_name" = gsub("\\.", "-", SampleIDs)
    ) %>%
    dplyr::select(
      "sample_name",
      "Immune_Subtype"
    ) %>%
    tidyr::pivot_longer(- "sample_name",
                        names_to = "parent_tag",
                        values_to = "tag_name") %>%
    dplyr::select(
      "sample_name",
      "parent_tag",
      "tag_name"
    )

  tide_result <- synapse_tsv_id_to_tbl(syn, "syn67716488") %>%
    dplyr::mutate(
      "TIDE_Responder" = dplyr::if_else(
        Responder == FALSE,
        "false_tide_responder",
        "true_tide_responder"
      ),
      "TIDE_No_Benefits" = dplyr::if_else(
        `No benefits` == FALSE,
        "false_tide_no_benefits",
        "true_tide_no_benefits"
      )
    ) %>%
    dplyr::select(
      "sample_name" = "...1",
      "TIDE_Responder",
      "TIDE_No_Benefits"
    ) %>%
    tidyr::pivot_longer(- "sample_name",
                        names_to = "parent_tag",
                        values_to = "tag_name") %>%
    dplyr::select(
      "sample_name",
      "parent_tag",
      "tag_name"
    )



  #now we are ready to add all clinical annotation in one table, pivot it to a long format, and then add sample and tag ids
  samples_to_tags <-  clinical_df %>%
    dplyr::select(
      "sample_name",
      "gender" = "patient_gender",
      "race" = "patient_race",
      "ethnicity" = "patient_ethnicity" ,
      "Sample_Treatment",
      "ICI_Rx",
      "ICI_Pathway",
      "ICI_Target",
      "Non_ICI_Rx",
      "NeoICI_Rx",
      "Subsq_Rx",
      "Subsq_ICI_Rx",
      "Cancer_Tissue",
      "Tissue_Subtype",
      "Metastasized",
      "Clinical_Stage",
      "Biopsy_Site",
      "FFPE",
      "Responder",
      "Response",
      "Polyp_Histology",
      "Clinical_Benefit",
      "Progression",
      "TCGA_Study",
      "TCGA_Subtype",
    ) %>%
    tidyr::pivot_longer(-sample_name, names_to = "parent_tag", values_to = "tag_name")%>% #convert table to long format
    dplyr::bind_rows(immune_subtypes) %>%
    dplyr::bind_rows(tide_result) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(samples, by = "sample_name") %>% #get samples ids
    dplyr::inner_join(tags, by = "tag_name") %>% #get tags ids. Please note: if an annotation is not in the tags dataframe, it'll be deleted in this step. You need to make sure that all annotations are stored in a tags table
    dplyr::select("tag_id", "sample_id") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    samples_to_tags,
    "syn66227466",
    "samples_to_tags"
  )

}
