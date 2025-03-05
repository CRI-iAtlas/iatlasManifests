samples_to_tags_amadeus <- function() {

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


  clinical_df <- synapse_csv_id_to_tbl(syn, "syn54074560") %>% #update with source of clinical info
    dplyr::mutate(
      "patient_name" = paste("AMADEUS", `Subject ID`, sep = "_")
    )

  patient_tags <- synapse_csv_id_to_tbl(syn, "") %>% #update with synapse id for patients table
    dplyr::mutate(
      "race" = dplyr::case_when(
        race == "asian" ~ "asian_race",
        race == "white" ~ "white_race",
        race == "other" ~ "other_race",
        race == "black or african american" ~ "black_or_african_american_race",
        is.na(race) ~ "na_race"
      ),
      "ethnicity" = dplyr::case_when(
        ethnicity == "hispanic or latino" ~ "hispanic_or_latino_ethnicity",
        ethnicity == "not hispanic or latino" ~ "not_hispanic_or_latino_ethnicity",
        is.na(ethnicity) ~ "na_ethnicity"
      )
    ) %>%
    dplyr::rename(
      "patient_name" = "name",
      "patient_id" = "id")

  samples <-
    synapse_csv_id_to_tbl(syn, "") %>% # UPDATE with synapse id for samples table
    dplyr::inner_join(patient_tags, by = "patient_id") %>%
    dplyr::rename(
      "sample_name" = "name",
      "sample_id" = "id"
    )

  patient_tags <- samples %>%
    dplyr::select(sample_name, race, ethnicity) %>%
    tidyr::pivot_longer(- "sample_name",
                      names_to = "parent_tag",
                      values_to = "tag_name") %>%
    dplyr::select(
      "sample_name",
      "parent_tag",
      "tag_name"
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
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )

  tag_names <- #keep this from samples_to_tags_tcga
    synapse_feather_id_to_tbl(syn, "syn23545011" ) %>%
    dplyr::select("tag" = "old_name", "new_tag" = "name") %>%
    tidyr::drop_na()

  # This code show how this table was generated for the AMADEUS dataset.
  # Several annotations were available in different sources and below is the data wrangling performed to get them ready
  # You'll need to change this code according to the steps required to get the data


  #"Tumor_tissue_type" - get from full manifest

  patients_manifest <- synapse_tsv_id_to_tbl(syn, "syn64290150") %>%
    dplyr::bind_rows(synapse_tsv_id_to_tbl(syn, "syn64369373") ) %>%
    dplyr::mutate(
      "sample_name" = paste(paste(Dataset, Patient_Name, sep = "-"), Run_Name, sep = "-"),
      "Tumor_tissue_type" = dplyr::if_else(
         isTRUE(Normal),
         "normal_tumor_tissue_type",
         "metastatic_tumor_tissue_type",
      )
    ) %>%
    dplyr::select(sample_name, Tumor_tissue_type) %>%
    dplyr::filter(sample_name %in% samples$sample_name) %>%
    tidyr::pivot_longer(- "sample_name",
                        names_to = "parent_tag",
                        values_to = "tag_name") %>%
    dplyr::select(
      "sample_name",
      "parent_tag",
      "tag_name"
    )

  immune_subtypes <- synapse_csv_id_to_tbl(syn, "") %>% #update with synapse id for results for immune subtype classifier
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

  tide_result <- synapse_tsv_id_to_tbl(syn, "") %>% #update with synapse id for results for TIDE scores
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


  ##sample collection timepoint is available from batch info
  samples_annotation <- synapse_tsv_id_to_tbl(syn, "syn54033036") %>% #file with clinical info
    dplyr::mutate(
      "sample_name" = gsub("/", "_", paste0(paste("AMADEUS_PERSONALIS_Batch-TXMonly", `Participant ID`, sep = "-"), paste("-ar", `Personalis Sample Name/Specimen ID`, sep = "_"))),
      "Sample_Collection_Timepoint" =  dplyr::case_when(
        Visit == "BL" ~ "pre_sample_treatment",
        Visit == "OT1" ~ "on_sample_treatment"
      )
    ) %>%
    dplyr::select(
      "sample_name",
      "Sample_Collection_Timepoint"
    )

  # prior rx are available in table from github repo: https://github.com/ParkerICI/amadeus-trial-data/blob/main/clinical-data/AMADEUS_primarycohort_priortherapy.csv
  ici_drugs <- c("ATEZOLIZUMAB","DURVALUMAB","NIVOLUMAB","PEMBROLIZUMAB","SPARTALIZUMAB", "TREMELIMUMAB", "VOPRATELIMAB")
  patterns_to_remove <- c(";", " ", ", ")

  prior_rx <- synapse_csv_id_to_tbl(syn, "syn64420878") %>%
    dplyr::mutate(
      "patient_name" = paste0("AMADEUS_",
                              ifelse(subject.id == "104-0013/104-0022",
                                     "104-0022",
                                     sub("_.*$", "",  subject.id))),
      "parent_tag" = dplyr::case_when(
        stringr::str_detect(therapy.coded.preferred.name, paste0(ici_drugs, collapse = '|')) ~ "Prior_ICI_Rx",
        !stringr::str_detect(therapy.coded.preferred.name, paste0(ici_drugs, collapse = '|')) ~ "Prior_Rx"
      )
    ) %>%
    dplyr::group_by(patient_name, parent_tag) %>%
    dplyr::summarise(
      all_drugs = paste0(sort(tolower(unique(therapy.coded.preferred.name))), collapse = ", "),
      tag_name = dplyr::if_else(
        parent_tag == "Prior_ICI_Rx",
        paste0(gsub(", ", "_", all_drugs), "_prior_ici_rx"),
        paste0((stringr::str_replace_all(all_drugs, paste0(patterns_to_remove, collapse = '|'), "_")), "_prior_rx")
      ),
      .groups = "drop"
    ) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(samples, by = "patient_name", relationship = "many-to-many") %>%
    dplyr::select(sample_name, parent_tag, tag_name)


  #now we are ready to add all clinical annotation in one table, pivot it to a long format, and then add sample and tag ids
  samples_to_tags <-  samples %>%
    dplyr::inner_join(clinical_df, by = dplyr::join_by("patient_name")) %>%
    dplyr::inner_join(samples_annotation, by = "sample_name") %>%
    dplyr::distinct() %>%
    dplyr::mutate( #here we are creating columns with annotation using the tag_names that are stored in the database. You can make changes as needed
      "gender" = "male",
      "Response" = dplyr::case_when(
        `Best Overall Response` == "Progressive Disease" ~ "progressive_disease_response",
        `Best Overall Response` == "Stable Disease" ~ "stable_disease_response",
        `Best Overall Response` == "Partial Response" ~ "partial_response_response",
        `Best Overall Response` == "Complete Response" ~ "complete_response_response",
        is.na(`Best Overall Response`) ~ "na_response",
      ),
      "Responder" = dplyr::case_when(
        `Best Overall Response` %in% c("Progressive Disease", "Stable Disease") ~ "false_responder",
        `Best Overall Response`  %in% c("Complete Response", "Partial Response") ~ "true_responder",
        is.na(`Best Overall Response`) ~ "na_responder",
      ),
      "Progression" = dplyr::case_when(
        `Best Overall Response` %in% c("Complete Response", "Partial Response", "Stable Disease") ~ "false_progression",
        `Best Overall Response`  %in% "Progressive Disease" ~ "true_progression",
        is.na(`Best Overall Response`) ~ "na_progression",
      ),
      "Clinical_Benefit" = dplyr::case_when(
        `Best Overall Response` %in% c("Complete Response", "Partial Response", "Stable Disease") ~ "true_clinical_benefit",
        `Best Overall Response`  %in% "Progressive Disease" ~ "false_clinical_benefit",
        is.na(`Best Overall Response`) ~ "na_clinical_benefit",
      ),
      "Metastasized" = "true_metastasized",
      "ICI_Rx" =   dplyr::case_when(
          Treatment == "Nivolumab" ~ "nivolumab",
          Treatment == "Nivolumab + Ipilimumab" ~ "ipilimumab_nivolumab"
        ),
      "ICI_Pathway" =  dplyr::case_when(
        Treatment == "Nivolumab" ~ "pd1_ici_pathway",
        Treatment == "Nivolumab + Ipilimumab" ~ "ctla4_pd1_ici_pathway"
      ),
      "ICI_Target" =  dplyr::case_when(
        Treatment == "Nivolumab" ~ "pd1_ici_target",
        Treatment == "Nivolumab + Ipilimumab" ~ "ctla4_pd1_ici_target"
      ),
      "Non_ICI_Rx" = "na_non_ici_rx",
      "NeoICI_Rx" = "none_neoici_rx",
      "Subsq_Rx" = 'unknown_subsq_rx',
      "Subsq_ICI_Rx" =  dplyr::if_else(
        is.na(`Crossover to Ipi+Nivo`),
        "na_subsq_ici_rx",
        "ipilimumab_nivolumab_subsq_ici_rx"
      ),
      "AMADEUS_Study" = dplyr::case_when(
        `Tumor Type` == "Breast" ~ "BRCA_amadeus",
        `Tumor Type` == "Prostate" ~  "CRPC_amadeus",
        `Tumor Type` ==  "Non-small Cell Lung Cancer" ~ "LUCA_amadeus",
        `Tumor Type` ==  "Head and Neck" ~ "HNCA_amadeus",

        is.na(`Tumor Type`) ~ "na_amadeus"
      ),
      "TCGA_Study" = dplyr::case_when(
        `Tumor Type` == "Breast" ~ "BRCA",
        `Tumor Type` == "Prostate" ~  "CRPC",
        is.na(`Tumor Type`) ~ "na_tcga_study"
      ),
      "TCGA_Subtype" = "na_tcga_subtype",
      "Cancer_Tissue" = dplyr::case_when(
        `Tumor Type` == "Breast" ~ "breast_cancer_tissue",
        `Tumor Type` == "Prostate" ~  "prostate_cancer_tissue",
        `Tumor Type` ==  "Non-small Cell Lung Cancer" ~ "lungs_cancer_tissue",
        `Tumor Type` == "Renal" ~ "kidney_cancer_tissue",
        `Tumor Type` == "Lung" ~ "lungs_cancer_tissue",
      ),
      "Tissue_Subtype" = "na_tissue_subtype",
      "Clinical_Stage" = "iv_clinical_stage",
      "Polyp_Histology" = "na_polyp_histology",
      "FFPE" = "true_ffpe",
      "Biopsy_Site" = "pbmc_biopsy_site" #from methods description
    ) %>%
    dplyr::select(
      "sample_name",
      "gender",
      "race",
      "ethnicity",
      "Sample_Collection_Timepoint",
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
      "AMADEUS_Study",
      "TCGA_Study",
      "TCGA_Subtype",
    ) %>%
    tidyr::pivot_longer(-sample_name, names_to = "parent_tag", values_to = "tag_name")%>% #convert table to long format
    dplyr::bind_rows(patients_manifest) %>%
    dplyr::bind_rows(prior_rx) %>%
    dplyr::bind_rows(patient_tags) %>%
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
    "", #update
    "samples_to_tags"
  )

}
