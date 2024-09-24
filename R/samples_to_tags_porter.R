samples_to_tags_porter <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  clinical_df <- synapse_csv_id_to_tbl(syn, "syn54031467")

  ffpe_tumor_annotation <- synapse_tsv_id_to_tbl(syn, "syn54022320")

  # samples <-
  #   synapse_csv_id_to_tbl(syn, "syn51589463") %>%
  #   dplyr::select(
  #     "sample_name" = "name",
  #     "sample_id" = "id"
  #   )
  #
  # tags <-
  #   synapse_csv_id_to_tbl(syn, "syn51613683") %>% #ici specific tags
  #   dplyr::add_row(
  #     synapse_csv_id_to_tbl(syn, "syn51080176") #add tags from tcga
  #   ) %>%
  #   dplyr::select(
  #     "tag_name" = "name",
  #     "tag_id" = "id"
  #   )
  #
  # tag_names <- #keep this from samples_to_tags_tcga
  #   synapse_feather_id_to_tbl(syn, "syn23545011" ) %>%
  #   dplyr::select("tag" = "old_name", "new_tag" = "name") %>%
  #   tidyr::drop_na()


  samples_to_tags <- clinical_df %>%
    dplyr::mutate(
      #"sample_name" = trimws(paste0("Krishna_ccRCC_", sample_name)),
      "race" = dplyr::case_when(
        Race == "Asian" ~ "asian_race",
        Race == "White" ~ "white_race",
        Race == "Other" ~ "other_race",
        Race == "Black or African American" ~ "black_or_african_american_race",
      ),
      "ethnicity" = dplyr::case_when(
        Ethnicity == "Hispanic or Latino" ~ "hispanic_or_latino_ethnicity",
        Ethnicity == "Not Hispanic or Latino" ~ "not_hispanic_or_latino_ethnicity",
      ),
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
      "Sample_Collection_Timepoint" = "na_sample_treatment",
      # "Biopsy_Site" = Biopsy.Site, #these 3 are in the original metadata for each batch syn54019614, syn54028226
      # "Tumor_tissue_type" = dplyr::case_when(
      #   Tumor_tissue_type == "tumor" ~ "primary_tumor_tissue_type",
      #   Tumor_tissue_type == "normal_tumor_tissue_type" ~ "normal_tumor_tissue_type",
      #   is.na(Tumor_tissue_type) ~ "na_tumor_tissue_type"
      # ),
      # "FFPE" = "false_ffpe",
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
    "",
    "samples_to_tags"
  )

}
