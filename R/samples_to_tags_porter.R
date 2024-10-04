samples_to_tags_porter <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  clinical_df <- synapse_csv_id_to_tbl(syn, "syn54031467")

  ffpe_tumor_annotation <- synapse_tsv_id_to_tbl(syn, "syn54022320")

  batch1 <- synapse_tsv_id_to_tbl(syn, "syn54019614") %>% #file with clinical info
    dplyr::mutate(
      "sample_name" = paste0(paste("PORTER_PERSONALIS_BATCH1", `Screening ID`, sep = "-"), paste("-ar", `Sample Name`, sep = "_")),
      "Tumor_tissue_type" = dplyr::case_when(
        `Tumor or Normal?` == "Tumor" ~ "metastatic_tumor_tissue_type", #from clinical trials data, we know collected tumor samples are metastatic
        `Tumor or Normal?` == "Normal" ~ "normal_tumor_tissue_type",
        ),
      "Sample_Collection_Timepoint" =  dplyr::case_when(
        Visit %in% c("BL", "Archival") ~ "pre_sample_treatment",
        Visit %in% c("On Treatment", "C2D1", "C1D8", "C4D1") ~ "on_sample_treatment",
        Visit %in% c("EOT", "PD") ~ "post_sample_treatment"
      ),
      "Biopsy_Site" =  dplyr::case_when(
        `Sample Type` == "PBMC EDTA" ~ "pbmc_biopsy_site",
        `Sample Type` %in% c("Unstained Slide", "Frozen Tissue") ~ "unknown_biopsy_site",
      ),
      "FFPE" =  dplyr::case_when(
        `Sample Type`%in% c("PBMC EDTA", "Frozen Tissue") ~ "false_ffpe",
        `Sample Type` == "Unstained Slide" ~ "true_ffpe",
      )
  ) %>%
  dplyr::select(
    "sample_name",
    "Tumor_tissue_type",
    "Sample_Collection_Timepoint",
    "Biopsy_Site",
    "FFPE"
  )

  batch2 <- synapse_tsv_id_to_tbl(syn, "syn54022320") %>%
    tidyr::drop_na("warehouse") %>%
    dplyr::mutate(
      "sample_name" = paste0(paste("PORTER_PERSONALIS_BATCH2", `Participant ID`, sep = "-"), paste("-ar", `Participant Sample`, sep = "_")),
      "Tumor_tissue_type" = dplyr::case_when(
        `DNA data?` == "DNA" ~ "normal_tumor_tissue_type",  #from files names, we infer that only the DNA files are from normal samples
        is.na(`DNA data?`) ~ "metastatic_tumor_tissue_type",
      ),
      "Sample_Collection_Timepoint" =  dplyr::case_when(
        Visit %in% c("BL", "Archival") ~ "pre_sample_treatment",
        Visit %in% c("On Treatment", "C1D1", "C2D1", "C1D8", "C4D1") ~ "on_sample_treatment",
        Visit %in% c("EOT", "PD") ~ "post_sample_treatment"
      ),
      "Biopsy_Site" =  dplyr::case_when(
        `type` == "pbmc" ~ "pbmc_biopsy_site",
        `type` == "ffpe"~ "unknown_biopsy_site",
      ),
      "FFPE" =  dplyr::case_when(
        `type` == "pbmc" ~ "false_ffpe",
        `type` == "ffpe" ~ "true_ffpe",
      )
    ) %>%
    dplyr::select(
      "sample_name",
      "Tumor_tissue_type",
      "Sample_Collection_Timepoint",
      "Biopsy_Site",
      "FFPE"
    )

  batch3 <- synapse_tsv_id_to_tbl(syn, "syn54028226")%>%
    dplyr::mutate(
      "sample_name" = paste0(paste("PORTER_PERSONALIS_BATCH3", `Participant ID`, sep = "-"), paste("-ar", `Sample Name`, sep = "_")),
      "Tumor_tissue_type" = dplyr::case_when(
        `tumor or normal` == "tumor" ~ "metastatic_tumor_tissue_type", #from clinical trials data, we know collected tumor samples are metastatic
        `tumor or normal` == "normal" ~ "normal_tumor_tissue_type",
      ),
      "Sample_Collection_Timepoint" =  dplyr::case_when(
        Visit %in% c("BL", "Archival") ~ "pre_sample_treatment",
        Visit %in% c("A", "OT") ~ "na_sample_treatment"
      ),
      "Biopsy_Site" =  dplyr::case_when(
        `type` == "pbmc" ~ "pbmc_biopsy_site",
        `type` == "ffpe"~ "unknown_biopsy_site",
      ),
      "FFPE" =  dplyr::case_when(
        `type` == "pbmc" ~ "false_ffpe",
        `type` == "ffpe" ~ "true_ffpe",
      )
    ) %>%
    dplyr::select(
      "sample_name",
      "Tumor_tissue_type",
      "Sample_Collection_Timepoint",
      "Biopsy_Site",
      "FFPE"
    )

  samples_annotation <- dplyr::bind_rows(batch1, batch2, batch3)


  patient_tags <- synapse_csv_id_to_tbl(syn, "syn63623064") %>%
    dplyr::rename(
      "patient_name" = "name",
      "patient_id" = "id")

  samples <-
    synapse_csv_id_to_tbl(syn, "syn63623078") %>%
    dplyr::inner_join(patient_tags, by = "patient_id") %>%
    dplyr::rename(
      "sample_name" = "name",
      "sample_id" = "id"
    )

  immune_subtypes <- synapse_csv_id_to_tbl(syn, "syn63562157") %>%
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
                        values_to = "tag") %>%
    dplyr::select(
      "sample_name",
      "tag"
    )

  tide_result <- synapse_tsv_id_to_tbl(syn, "syn63562158") %>%
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
                        values_to = "tag") %>%
    dplyr::select(
      "sample_name",
      "tag"
    )

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
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )

  tag_names <- #keep this from samples_to_tags_tcga
    synapse_feather_id_to_tbl(syn, "syn23545011" ) %>%
    dplyr::select("tag" = "old_name", "new_tag" = "name") %>%
    tidyr::drop_na()


  samples_to_tags <-  samples %>%
    dplyr::inner_join(clinical_df, by = dplyr::join_by("patient_name" == "Subject ID")) %>%
    dplyr::inner_join(samples_annotation, by = "sample_name") %>%
    dplyr::mutate(
      "gender" = "male",
      "Response" = dplyr::case_when(
        `Best Overall Response` == "PROGRESSIVE DISEASE" ~ "progressive_disease_response",
        `Best Overall Response` == "STABLE DISEASE" ~ "stable_disease_response",
        `Best Overall Response` == "PARTIAL RESPONSE" ~ "partial_response_response",
        `Best Overall Response` == "NOT EVALUABLE" ~ "na_response",
      ),
      "Responder" = dplyr::case_when(
        `Best Overall Response` %in% c("PROGRESSIVE DISEASE", "STABLE DISEASE") ~ "false_responder",
        `Best Overall Response` == "PARTIAL RESPONSE" ~ "true_responder",
        `Best Overall Response` == "NOT EVALUABLE" ~ "na_responder",
      ),
      "Progression" = dplyr::case_when(
        `Best Overall Response` %in% c("PARTIAL RESPONSE", "STABLE DISEASE") ~ "false_progression",
        `Best Overall Response` == "PROGRESSIVE DISEASE" ~ "true_progression",
        `Best Overall Response` == "NOT EVALUABLE" ~ "na_progression",
      ),
      "Clinical_Benefit" = dplyr::case_when(
        `Best Overall Response` %in% c("PARTIAL RESPONSE", "STABLE DISEASE") ~ "true_clinical_benefit",
        `Best Overall Response` == "PROGRESSIVE DISEASE" ~ "false_clinical_benefit",
        `Best Overall Response` == "NOT EVALUABLE" ~ "na_clinical_benefit",
      ),
      "Metastasized" = "true_metastasized",
      "TCGA_Study" = "CRPC",
      "TCGA_Subtype" = "CRPC_mCRPC",
      "ICI_Rx" = "nivolumab",
      "ICI_Pathway" =  "pd1_ici_pathway",
      "ICI_Target" = "pd1_ici_target",
      "Non_ICI_Rx" = dplyr::case_when(
        Treatment == "NKTR-214 + Nivolumab" ~ "NKTR_214_non_ici_rx",
        Treatment == "SBRT + CDX-301 + poly-ICLC + Nivolumab" ~ "SBRT_CDX_301_poly_ICLC_non_ici_rx",
        Treatment == "CDX-301 + INO-5151 + Nivolumab" ~ "CDX_301_INO_5151_non_ici_rx"
      ),
      "NeoICI_Rx" = "none_neoici_rx",
      "Prior_Rx" = "unnamed_prior_rx",
      "Prior_ICI_Rx" = "unknown_prior_ici_rx",
      "Subsq_Rx" = 'unknown_subsq_rx',
      "Subsq_ICI_Rx" = "unknown_subsq_ici_rx",
      "Cancer_Tissue" = "prostate_cancer_tissue",
      "Tissue_Subtype" = "na_tissue_subtype",
      "Clinical_Stage" = "na_clinical_stage",
      "Polyp_Histology" = "na_polyp_histology",
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
      "Prior_Rx",
      "Prior_ICI_Rx",
      "Subsq_Rx",
      "Subsq_ICI_Rx",
      "Cancer_Tissue",
      "Tissue_Subtype",
      "Metastasized",
      "Clinical_Stage",
      "Biopsy_Site",
      "Tumor_tissue_type",
      "FFPE",
      "Responder",
      "Response",
      "Polyp_Histology",
      "Clinical_Benefit",
      "Progression",
      "TCGA_Study",
      "TCGA_Subtype",
    ) %>%
    tidyr::pivot_longer(-sample_name, names_to = "parent_tag", values_to = "tag_name")%>%
    dplyr::inner_join(samples, by = "sample_name") %>%
    dplyr::inner_join(tags, by = "tag_name") %>%
    dplyr::select("tag_id", "sample_id") %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    samples_to_tags,
    "syn63623056",
    "samples_to_tags"
  )

}
