prince_build_tag_table <- function(){

  #1. LOADING DATA
  ici_tags <- synapse_csv_id_to_tbl(syn, "syn51613683")

  demographics <- synapse_csv_id_to_tbl(syn, "syn51251905")
  samples <- synapse_csv_id_to_tbl(syn, "syn51252936")

  trial_drugs <- synapse_csv_id_to_tbl(syn, "syn51251938")

  # previous and post treatments
  radiation_pre <- synapse_csv_id_to_tbl(syn, "syn51251999")
  radiation_after <- synapse_csv_id_to_tbl(syn, "syn51252021")
  surgery_pre <- synapse_csv_id_to_tbl(syn, "syn51252008")
  surgery_after  <- synapse_csv_id_to_tbl(syn, "syn51252025")
  drugs_pre <- synapse_csv_id_to_tbl(syn, "syn51252006")
  drugs_after <- synapse_csv_id_to_tbl(syn, "syn51252023")

  #2. ORGANIZING DRUGS FOR EACH TRIAL ARM

  key_df <- dplyr::tibble("Arm" = character(),
                          "ICI_Rx" = character(),
                          "ICI_Pathway" = character(),
                          "ICI_Target" = character(),
                          "Non_ICI_Rx" = character()) %>%
    dplyr::add_row("Arm" = "A1",
                   "ICI_Rx" = "nivolumab",
                   "ICI_Pathway" = "pd1_ici_pathway",
                   "ICI_Target" = "pd1_ici_target",
                   "Non_ICI_Rx" = "nab-paclitaxel_gemcitabine_non_ici_rx") %>%
    dplyr::add_row("Arm" = "B1",
                   "ICI_Rx" = "none_ICI_Rx",
                   "ICI_Pathway" = "none_ici_pathway",
                   "ICI_Target" = "none_ICI_Target",
                   "Non_ICI_Rx" = "nab-paclitaxel_gemcitabine_APX005M_non_ici_rx")  %>%
    dplyr::add_row("Arm" = "B2",
                   "ICI_Rx" = "none_ICI_Rx",
                   "ICI_Pathway" = "none_ICI_Rx",
                   "ICI_Target" ="none_ICI_Rx",
                   "Non_ICI_Rx" = "nab-paclitaxel_gemcitabine_APX005M_non_ici_rx")  %>%
    dplyr::add_row("Arm" = "C1",
                   "ICI_Rx" = "nivolumab",
                   "ICI_Pathway" = "pd1_ici_pathway",
                   "ICI_Target" = "pd1_ici_target",
                   "Non_ICI_Rx" = "nab-paclitaxel_gemcitabine_APX005M_non_ici_rx") %>%
    dplyr::add_row("Arm" = "C2",
                   "ICI_Rx" = "nivolumab",
                   "ICI_Pathway" = "pd1_ici_pathway",
                   "ICI_Target" = "pd1_ici_target",
                   "Non_ICI_Rx" = "nab-paclitaxel_gemcitabine_APX005M_non_ici_rx")

  #there are some patients in the B2 arm that have records of having taking nivolumab
  b2_with_nivo <- trial_drugs %>%
    tidyr::fill(`Subject ID`, Arm) %>%
    dplyr::filter(Arm == "B2", Drug == "Nivolumab", !is.na(`Dose \r\n Administered \r\n (mg)`))

  #3. ORGANIZING PRE AND POST TRIAL TREATMENTS

  #treatments before trial
  rem_dup_word <- function(x){
    x <- paste0(tolower(x), collapse = "_")
    x <- gsub(";", "_", x)

    x <- paste0(sort(unique(trimws(unlist(strsplit(x,split="_",fixed=F,perl=T))))),collapse ="_")
    gsub(" ", "_", x)
  }

  pre_drugs <- drugs_pre %>%
    tidyr::fill(`Subject ID`) %>%
    dplyr::group_by(`Subject ID`) %>%
    dplyr::summarise(drugs = rem_dup_word(`Name of Therapy`))

  pre_treatment <- (surgery_pre %>% dplyr::select("Subject ID") %>% dplyr::distinct() %>% dplyr::mutate(surgery = "surgery")) %>%
    dplyr::full_join(
      radiation_pre %>% select("Subject ID") %>% dplyr::distinct() %>% dplyr::mutate(radiation = "radiation"),
      by = dplyr::join_by(`Subject ID`)
    ) %>%
    dplyr::full_join(
      pre_drugs,
      by = dplyr::join_by(`Subject ID`)
    ) %>%
    dplyr::filter(!is.na(`Subject ID`)) %>%
    tidyr::unite(
      "Prior_Rx", surgery:drugs, sep = "_", na.rm = TRUE
    )

  #treatments after trial
  #some patients had ICI after the trial

  ici_drugs_after <- c("PEMBROLIZUMAB", "DURVALUMAB")

  post_drugs <- drugs_after %>%
    dplyr::filter(!`Name of Therapy` %in% ici_drugs_after) %>%
    tidyr::fill(`Subject ID`) %>%
    dplyr::group_by(`Subject ID`) %>%
    dplyr::summarise(drugs = rem_dup_word(`Name of Therapy`))

  post_treatment <- (surgery_after %>% dplyr::select("Subject ID") %>% dplyr::distinct() %>% dplyr::mutate(surgery = "surgery")) %>%
    dplyr::full_join(
      radiation_after %>% select("Subject ID") %>% dplyr::distinct() %>% dplyr::mutate(radiation = "radiation"),
      by = dplyr::join_by(`Subject ID`)
    ) %>%
    dplyr::full_join(
      post_drugs,
      by = dplyr::join_by(`Subject ID`)
    ) %>%
    dplyr::filter(!is.na(`Subject ID`)) %>%
    tidyr::unite(
      "Subsq_Rx", surgery:drugs, sep = "_", na.rm = TRUE
    )

  post_ici_treatment <- drugs_after %>%
    tidyr::fill(`Subject ID`) %>%
    dplyr::filter(`Name of Therapy` %in% ici_drugs_after)
  post_ici_treatment <- setNames(paste0(tolower(post_ici_treatment$`Name of Therapy`), "_subsq_ici_rx"), post_ici_treatment$`Subject ID`)

  pre_post_treatment <- dplyr::full_join(
    pre_treatment,
    post_treatment,
    by = dplyr::join_by(`Subject ID`)
  )

  #4. MERGING EVERYTHING

  prince_tags <- samples %>%
    dplyr::inner_join(
      subjects, by = "subject.id"
    ) %>%
    dplyr::inner_join(
      timepoints, by = "timepoint.id"
    ) %>%
    dplyr::inner_join(
      demographics, by = dplyr::join_by(subject.id == "Subject ID")
    ) %>%
    dplyr::inner_join(
      key_df, by = dplyr::join_by(subject.therapies == "Arm")
    ) %>%
    dplyr::inner_join(
      pre_post_treatment, by = dplyr::join_by(subject.id == "Subject ID" )
    ) %>%
    dplyr::mutate(
      "Response" = dplyr::case_when(
        bor == "SD" ~ "stable_disease_response",
        bor == "PD" ~ "progressive_disease_response",
        bor == "PR" ~ "partial_response_response",
        bor == "CR" ~ "complete_response_response",
        is.na(bor) ~ "na_response"
      ),
      "Responder" = dplyr::case_when(
        bor %in% c("PR","CR" )  ~ "true_responder",
        bor %in% c("PD","SD" )  ~ "false_responder",
        is.na(bor) ~ "na_responder"
      ),
      "Progression" = dplyr::case_when(
        bor %in% c("PD")  ~ "true_progression",
        bor %in% c("PR","CR","SD")  ~ "false_progression",
        is.na(bor) ~ "na_progression"
      ),
      "Clinical_Benefit" = dplyr::case_when(
        bor %in% c("PD")  ~ "false_clinical_benefit",
        bor %in% c("PR","CR","SD")  ~ "true_clinical_benefit",
        is.na(bor) ~ "na_clinical_benefit"
      ),
      "Biopsy_Site" = stringr::str_replace_all(
        gsub(" ", "_", tolower(paste0(.data$gdc.anatomic.site.name, "_biopsy_site"))),
        "right_middle_lobe,", ""
      ),
      "FFPE" = dplyr::if_else(
        stringr::str_detect(.data$sample.specimen, "ffpe"),
        "true_ffpe",
        "false_ffpe",
        missing = "na_ffpe"
      ),
      "Sample_Treatment" = dplyr::case_when(
        timepoint.type == ":timepoint.type/baseline" ~ "pre_sample_treatment",
        timepoint.type == ":timepoint.type/on-treatment" ~ "on_sample_treatment",
        timepoint.type == ":timepoint.type/eot" ~ "post_sample_treatment"
      ),
      "Clinical_Stage" = dplyr::case_when(
        `Stage at Initial Diagnosis` == "Stage 1" ~ "I",
        `Stage at Initial Diagnosis` == "Stage 2" ~ "II",
        `Stage at Initial Diagnosis` == "Stage 3" ~ "III",
        `Stage at Initial Diagnosis` == "Stage 4" ~ "IV"
      ),
      "ICI_Rx" = dplyr::if_else(
        .data$subject.id %in% b2_with_nivo$`Subject ID`,
        "nivolumab",
        .data$ICI_Rx
      ),
      "ICI_Pathway" = dplyr::if_else(
        .data$subject.id %in% b2_with_nivo$`Subject ID`,
        "pd1_ici_pathway",
        .data$ICI_Pathway
      ),
      "ICI_Target" = dplyr::if_else(
        .data$subject.id %in% b2_with_nivo$`Subject ID`,
        "pd1_ici_target",
        .data$ICI_Target
      ),
      "Prior_ICI_Rx" = "none_prior_ici_rx",
      "Subsq_ICI_Rx" = dplyr::if_else(
        subject.id %in% names(post_ici_treatment),
        post_ici_treatment[subject.id],
        "none_subsq_ici_rx"
      ),
      "Prior_Rx" = dplyr::if_else(
        is.na(.data$Prior_Rx),
        "none_prior_rx",
        .data$Prior_Rx
      ),
      "Subsq_Rx" = dplyr::if_else(
        is.na(.data$Subsq_Rx),
        "none_subsq_rx",
        .data$Subsq_Rx
      ),
      "Tissue_Subtype" = gsub(" ", "_", tolower(paste0(.data$`Cancer Location`, "_tissue_subtype"))),
      "Cancer_Tissue" = "pancreas_cancer_tissue",
      "NeoICI_Rx" = "none_neoici_rx",
      "Metastasized" = "true_metastasized"
    ) %>%
    dplyr::select(
      "sample.id",
      "subject.id",
      Sample_Treatment,
      Response,
      Responder,
      Progression,
      Clinical_Benefit,
      ICI_Rx,
      ICI_Pathway,
      ICI_Target,
      Non_ICI_Rx,
      NeoICI_Rx,
      Prior_Rx,
      Prior_ICI_Rx,
      Subsq_Rx,
      Subsq_ICI_Rx,
      Biopsy_Site,
      FFPE,
      Clinical_Stage,
      Tissue_Subtype,
      Cancer_Tissue,
      Metastasized
    )
}
