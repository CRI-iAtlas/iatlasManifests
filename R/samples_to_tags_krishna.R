samples_to_tags_krishna <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  #getting ids
  samples <-
    synapse_csv_id_to_tbl(syn, "syn59204288") %>%
    dplyr::select(
      "sample_name" = "name",
      "sample_id" = "id"
    )

  tags <-
    synapse_csv_id_to_tbl(syn, "syn53698018") %>% #msk tags
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn59210643") #krishna tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn58896103") #shiao tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn53697423") #vanderbilt tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn51613683") #ici tags
    ) %>%
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn51080176")  #add tags from TCGA
    ) %>%
    dplyr::select(
      "tag_name" = "name",
      "tag_id" = "id"
    )
  # the clinical annotation was manually annotated and saved in a file
  clinical_df <- syn$get("syn59202673") %>%
    purrr::pluck("path") %>%
    openxlsx::read.xlsx(., sheet = 1) %>%
    dplyr::as_tibble() %>%
    dplyr::select(-c("manuscript_name", "patient_name", "Age", "race", "ethnicity", "OS", "OS_time", "PFI", "PFI_time", "Dataset"))


  #consolidating
  samples_to_tags <- clinical_df %>%
    dplyr::mutate( #these categories are defined by the study protocol
      "sample_name" = trimws(paste0("Krishna_ccRCC_", sample_name)),
      "race" = "na_race",
      "ethnicity" = "na_ethnicity",
      "Sample_Collection_Timepoint" = paste0(Sample.Collection.Timepoint, "_sample_treatment"),
      "ICI_Rx" = dplyr::if_else(ICI.Drug == "none", "none_ICI_Rx", "nivolumab"),
      "ICI_Pathway" = dplyr::if_else(ICI.Pathway == "none", "none_ici_pathway", "pd1_ici_pathway"),
      "ICI_Target" = dplyr::if_else(ICI.Pathway == "none", "none_ICI_Target", "pd1_ici_target"),
      "Non_ICI_Rx" = dplyr::case_when(
        `Non-ICI.drugs.during.treatment` == "na" ~ "na_non_ici_rx",
        `Non-ICI.drugs.during.treatment` == "none" ~ "none_non_ici_rx",
        `Non-ICI.drugs.during.treatment` == "surgery" ~ "surgery_non_ici_rx"
      ),
      "NeoICI_Rx" = dplyr::if_else(ICI.as.Neoadjuvant == "none", "none_neoici_rx", "nivolumab_neoici_rx"),
      "Prior_Rx" = dplyr::if_else(Treatment.prior.to.ICI == "none", "none_prior_rx", "surgery_prior_rx"),
      "Prior_ICI_Rx" = dplyr::case_when(
        Prior.treatment.with.ICI == "none" ~ "none_prior_ici_rx",
        Prior.treatment.with.ICI == "nivo" ~ "nivolumab_prior_ici_rx",
        Prior.treatment.with.ICI == "ipi/nivo" ~ "ipilimumab_nivolumab_prior_ici_rx",
        Prior.treatment.with.ICI == "sunitinib, ipi/nivo" ~ "sunitinib_ipilimumab_nivolumab_prior_ici_rx",
      ),
      "Subsq_Rx" = dplyr::if_else(Treatment.after.ICI == "none", "none_subsq_rx", "surgery_subsq_rx"),
      "Subsq_ICI_Rx" = dplyr::if_else(Post.treatment.with.ICI == "none", "none_subsq_ici_rx", "ipilimumab_nivolumab_subsq_ici_rx"),
      "Cancer_Tissue" = "kidney_cancer_tissue",
      "Tissue_Subtype" = "na_tissue_subtype",
      "Metastasized" = Metastasized,
      "Clinical_Stage" = Clinical.Stage,
      "Biopsy_Site" = Biopsy.Site,
      "Tumor_tissue_type" = dplyr::case_when(
        Tumor_tissue_type == "tumor" ~ "primary_tumor_tissue_type",
        Tumor_tissue_type == "normal_tumor_tissue_type" ~ "normal_tumor_tissue_type",
        is.na(Tumor_tissue_type) ~ "na_tumor_tissue_type"
      ),
      "FFPE" = "false_ffpe",
      "Responder" = dplyr::if_else(Responder == "na", "na_responder", Responder),
      "Response" = "na_response",
      "Polyp_Histology" = "na_polyp_histology",
      "Clinical_Benefit" = "na_clinical_benefit",
      "Progression" = "na_progression",
      "TCGA_Study" = "RCC",
      "TCGA_Subtype" = "RCC_ccRCC"
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
    "syn59401061",
    "samples_to_tags"
  )

}
