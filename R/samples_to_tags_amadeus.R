samples_to_tags_amadeus <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  #TODO: add biopsy site from biopsy table in Github

  clinical_df <- synapse_csv_id_to_tbl(syn, "syn54074560") %>%
    dplyr::mutate(
      "patient_name" = paste("AMADEUS", `Subject ID`, sep = "_")
    )

  #sample collection timepoint is available from batch info
  batch <- synapse_tsv_id_to_tbl(syn, "syn54033036") %>% #file with clinical info
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

  batch1 <- synapse_tsv_id_to_tbl(syn, "syn54033308") %>%
    dplyr::mutate(
      "sample_name" = paste0(paste("AMADEUS_PERSONALIS_Batch1_Request33", `Participant ID`, sep = "-"), paste("-ar", sub("/.*$", "", filename), sep = "_")),
      "Sample_Collection_Timepoint" =  dplyr::case_when(
        Visit %in% c("BL", "Archival") ~ "pre_sample_treatment",
        Visit %in% c("On Treatment", "C2D12", "C2D2", "C2D21", "C1D22", "C2D20", "C1D1") ~ "on_sample_treatment",
        Visit %in% c("EOT", "PD") ~ "post_sample_treatment"
      ),
    ) %>%
    dplyr::select(
      "sample_name",
      "Sample_Collection_Timepoint"
    )

  batch2 <- synapse_tsv_id_to_tbl(syn, "syn54041251") %>%
    dplyr::mutate(
      "sample_name" = paste0(paste("AMADEUS_PERSONALIS_Batch2_Request69", `Participant ID`, sep = "-"), paste("-ar", `Personalis Sample Name/Specimen ID`, sep = "_")),
      "Sample_Collection_Timepoint" =  dplyr::case_when(
        Visit == "BL" ~ "pre_sample_treatment",
        Visit %in% c( "OT1", "OT2", "C1D8", "C3D1", "C4D1") ~ "on_sample_treatment",
        Visit %in% c("EOT", "PD") ~ "post_sample_treatment"
      ),
    ) %>%
    dplyr::select(
      "sample_name",
      "Sample_Collection_Timepoint"
    )

  batch3 <- synapse_tsv_id_to_tbl(syn, "syn54062158") %>%
    dplyr::mutate(
      "sample_name" = paste0(paste("AMADEUS_PERSONALIS_Batch3-17650", `Sample_Name`, sep = "-"), paste("-ar", `Patient_ID`, sep = "_")),
      "Sample_Collection_Timepoint" =  "na_sample_treatment"
    ) %>%
    dplyr::select(
      "sample_name",
      "Sample_Collection_Timepoint"
    )

  batch3_20080 <- synapse_tsv_id_to_tbl(syn, "syn54069853") %>%
    dplyr::mutate(
      "sample_name" = paste0(paste("AMADEUS_PERSONALIS_Batch3_20080", `Sample_Name`, sep = "-"), paste("-ar", `Patient_ID`, sep = "_")),
      "Sample_Collection_Timepoint" =  "na_sample_treatment"
    ) %>%
    dplyr::select(
      "sample_name",
      "Sample_Collection_Timepoint"
    )

  batch3_20938 <- synapse_tsv_id_to_tbl(syn, "syn54070130") %>%
    dplyr::mutate(
      "sample_name" = paste0(paste("AMADEUS_PERSONALIS_BATCH3_20938", `Patient_ID`, sep = "-"), paste("-ar", `Sample_Name`, sep = "_")),
      "Sample_Collection_Timepoint" =  "na_sample_treatment"
    ) %>%
    dplyr::select(
      "sample_name",
      "Sample_Collection_Timepoint"
    )

  samples_annotation <- dplyr::distinct(dplyr::bind_rows(batch, batch1, batch2, batch3, batch3_20080, batch3_20938))


  patient_tags <- synapse_csv_id_to_tbl(syn, "syn64290704") %>%
    dplyr::mutate(
      "race" = dplyr::case_when(
        race == "asian" ~ "asian_race",
        race == "white" ~ "white_race",
        race == "other" ~ "other_race",
        race == "black or african American" ~ "black_or_african_american_race",
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
    synapse_csv_id_to_tbl(syn, "syn64290688") %>%
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

  immune_subtypes <- synapse_csv_id_to_tbl(syn, "syn64154322") %>%
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

  tide_result <- synapse_tsv_id_to_tbl(syn, "syn64154321") %>%
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

  # Biopsy site is available at table from: https://github.com/ParkerICI/amadeus-trial-data/blob/main/clinical-data/AMADEUS_primarycohort_biopsy.csv
  biopsy_df <- synapse_csv_id_to_tbl(syn, "syn64425737") %>%


  tags <-
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


  samples_to_tags <-  samples %>%
    dplyr::inner_join(clinical_df, by = dplyr::join_by("patient_name")) %>%
    dplyr::inner_join(samples_annotation, by = "sample_name") %>%
    dplyr::distinct() %>%
    dplyr::mutate(
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
        `Crossover to Ipi+Nivo` == "Y",
        "ipilimumab_nivolumab_subsq_ici_rx",
        "na_subsq_ici_rx"
      ),
      "AMADEUS_Study" = dplyr::case_when(
        `Tumor Type` == "Breast" ~ "BRCA_amadeus",
        `Tumor Type` == "Prostate" ~  "CRPC_amadeus",
        `Tumor Type` ==  "Non-small Cell Lung Cancer" ~ "LUCA_amadeus",
        `Tumor Type` ==  "Head and Neck" ~ "HNCA_amadeus",
        `Tumor Type` == "Sarcoma" ~ "SARC_amadeus",
        `Tumor Type` == "Renal" ~ "RNCA_amadeus",
        `Tumor Type` == "Thyroid" ~ "THYR_amadeus",
        `Tumor Type` == "Colorectal" ~ "CRCA_amadeus",
        `Tumor Type` == "Hepatocellular Carcinoma" ~ "HCCA_amadeus",
        `Tumor Type` == "Liver" ~ "HECH_amadeus",
        `Tumor Type` == "Neuroendocrine" ~ "NEUC_amadeus",
        `Tumor Type` == "Gynecologic" ~ "UTCA_amadeus",
        `Tumor Type` == "Urethral" ~ "URET_amadeus",
        `Tumor Type` == "Penile" ~ "PENC_amadeus",
        `Tumor Type` == "PAPILLA OF VATER" ~ "AMPV_amadeus",
        `Tumor Type` == "Retroperitoneal Teratoma" ~ "TERA_amadeus",
        `Tumor Type` == "Merkel Cell Carcinoma" ~ "NESK_amadeus",
        `Tumor Type` == "Pelvis" ~ "PELV_amadeus",
        `Tumor Type` == "Gastroesophageal Junction" ~ "GEJC_amadeus",
        `Tumor Type` == "Peritoneum" ~ "PRTC_amadeus",
        `Tumor Type` == "Gastric" ~ "GSCA_amadeus",
        `Tumor Type` == "Lung" ~ "LUCA_amadeus",
        `Tumor Type` == "Pancreatic"  ~ "PANC_amadeus",
        is.na(`Tumor Type`) ~ "na_amadeus"
      ),
      "TCGA_Study" = dplyr::case_when(
        `Tumor Type` == "Breast" ~ "BRCA",
        `Tumor Type` == "Prostate" ~  "CRPC",
        `Tumor Type` ==  "Non-small Cell Lung Cancer" ~ "NSCLC",
        `Tumor Type` ==  "Head and Neck" ~ "HNSC",
        `Tumor Type` == "Sarcoma" ~ "SARC",
        `Tumor Type` == "Hepatocellular Carcinoma" ~ "LIHC",
        `Tumor Type` == "Liver" ~ "CHOL",
        `Tumor Type` %in% c("Pancreatic", "Lung", "Gastric", "Peritoneum", "Gastroesophageal Junction", "Pelvis", "Merkel Cell Carcinoma", "Retroperitoneal Teratoma", "PAPILLA OF VATER", "Colorectal", "Renal", "Thyroid", "Neuroendocrine", "Gynecologic","Urethral", "Penile") ~ "na_tcga_study"
        is.na(`Tumor Type`) ~ "na_tcga_study"
      ),
      "TCGA_Subtype" = "na_tcga_subtype",
      "Cancer_Tissue" = dplyr::case_when(
        `Tumor Type` == "Breast" ~ "breast_cancer_tissue",
        `Tumor Type` == "Prostate" ~  "prostate_cancer_tissue",
        `Tumor Type` ==  "Non-small Cell Lung Cancer" ~ "lungs_cancer_tissue",
        `Tumor Type` == "Renal" ~ "kidney_cancer_tissue",
        `Tumor Type` == "Lung" ~ "lungs_cancer_tissue",
        `Tumor Type` == "Pancreatic"  ~ "pancreas_cancer_tissue",
        `Tumor Type` == "Merkel Cell Carcinoma" ~ "skin_cancer_tissue",
        `Tumor Type` == "Gastric" ~ "stomach_cancer_tissue",
        `Tumor Type` == "Gastroesophageal Junction" ~ "stomach_cancer_tissue",
        `Tumor Type` == "Sarcoma" ~ "na_cancer_tissue",
        `Tumor Type` ==  "Head and Neck" ~ "na_cancer_tissue",
        is.na(`Tumor Type`) ~ "na_cancer_tissue",
        `Tumor Type` == "Gynecologic" ~ "uterus_cancer_tissue",
        `Tumor Type` == "Colorectal" ~ "colon_rectum_cancer_tissue",
        `Tumor Type` == "Hepatocellular Carcinoma" ~ "liver_cancer_tissue",
        `Tumor Type` == "Liver" ~ "liver_cancer_tissue",
        `Tumor Type` == "Thyroid" ~ "thyroid_cancer_tissue",
        `Tumor Type` == "Pelvis" ~ "pelvis_cancer_tissue",
        `Tumor Type` == "Peritoneum" ~ "peritoneum_cancer_tissue",
        `Tumor Type` == "Neuroendocrine" ~ "neuroendocrine_cancer_tissue",
        `Tumor Type` == "PAPILLA OF VATER" ~ "duodenum_cancer_tissue"
      ),
      "Tissue_Subtype" = "na_tissue_subtype",
      "Clinical_Stage" = "iv_clinical_stage",
      "Polyp_Histology" = "na_polyp_histology",
      "FFPE" = "true_ffpe"
      #"Biopsy_Site" = "pbmc_biopsy_site"
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
      #"Biopsy_Site",
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
    tidyr::pivot_longer(-sample_name, names_to = "parent_tag", values_to = "tag_name")%>%
    dplyr::bind_rows(patients_manifest) %>%
    dplyr::bind_rows(prior_rx) %>%
    dplyr::bind_rows(patient_tags) %>%
    dplyr::bind_rows(immune_subtypes) %>%
    dplyr::bind_rows(tide_result) %>%
    dplyr::inner_join(samples, by = "sample_name") %>%
    dplyr::inner_join(tags, by = "tag_name") %>%
    dplyr::select("tag_id", "sample_id") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    samples_to_tags,
    "",
    "samples_to_tags"
  )

}
