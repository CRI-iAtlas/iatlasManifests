htan_create_wide_tag_table <- function(){

  syn <- create_synapse_login()
  synapseclient <- reticulate::import("synapseclient")

  #data extraction functions

  remove_ici_drugs_from_string <- function(string_to_clean, ici_drugs){
    if(is.na(string_to_clean)) NA_character_
    else gsub("^\\++|\\+$", "", stringr::str_remove_all(string_to_clean, paste0(ici_drugs, collapse = '|')))
  }

  paste_non_ici_drug_to_string <- function(string_to_clean, string_to_paste, ici_drugs){

    cleaned_string <- remove_ici_drugs_from_string(string_to_clean, ici_drugs)

    if(is.na(cleaned_string)) return(string_to_paste)
    if(is.na(string_to_paste)) cleaned_string
    if(nchar(cleaned_string) == 0) return(string_to_paste)
    else paste(string_to_paste, cleaned_string, sep = "_")
  }

  format_entry <- function(string_to_format, patterns_to_remove, add_underscore = FALSE){
    reg_remove <- stringr::str_remove(string_to_format, paste0(patterns_to_remove, collapse = '|') )

    if(add_underscore) tolower(gsub(" ", "_", reg_remove)) #remove white spaces
    else reg_remove
  }

  #MSK
  #1. LOADING DATA

  msk_files <- c("biospecimen"="syn39256250",
                  "demographics" ="syn39253974",
                  "diagnosis" = "syn39254633",
                  "followup" = "syn39255125",
                  "therapy" = "syn39255232",
                 "subtype" = "msk_sclc_subtype" #the SCLC subtype was only found at a h5ad file, so it was exported to a table stored in this repo
  )

  files_path <- paste("inst/", msk_files, ".csv", sep = "") #locally stored, ideally will change to read directly from synapse
  msk_df <-  purrr::map(files_path, read.csv)
  names(msk_df) <- names(msk_files)

  #2. ADJUSTING TREATMENT INFORMATION

  #listing the ICI drugs that were used in the treatment
  ici_agents <- c("pembrolizumab", "nivolumab", "ipilimumab", "durvalumab", "atezolizumab")

  ici_targets <- setNames(c("pd1", "pd1", "ctla4", "pdl1", "pdl1"), ici_agents)
  ici_pathways <- setNames(c("pd1", "pd1", "ctla4", "pd1", "pd1"), ici_agents)

  therapy_iatlas <- msk_df$biospecimen %>%
    dplyr::select(
      "HTAN.Biospecimen.ID",
      "HTAN.Parent.ID",
      "Collection.Days.from.Index"
    ) %>%
    dplyr::inner_join(dplyr::select(msk_df$therapy, HTAN.Participant.ID, Days.to.Treatment.Start, Days.to.Treatment.End, Therapeutic.Agents),
                      by = dplyr::join_by(HTAN.Parent.ID == HTAN.Participant.ID), relationship = "many-to-many") %>%
    dplyr::filter(!is.na(Days.to.Treatment.Start) & !is.na(Days.to.Treatment.End) & Therapeutic.Agents != "") %>%
    dplyr::mutate(
      Days.to.Treatment.End = ifelse(
        is.na(Days.to.Treatment.End),
        Days.to.Treatment.Start,
        Days.to.Treatment.End
      )
    ) %>%
    dplyr::mutate(
      "flag" = dplyr::case_when(
        (Collection.Days.from.Index < Days.to.Treatment.Start) & stringr::str_detect(Therapeutic.Agents, paste0(ici_agents, collapse = '|')) ~ "Subsq_ICI_info",
        (Collection.Days.from.Index < Days.to.Treatment.Start) & !stringr::str_detect(Therapeutic.Agents, paste0(ici_agents, collapse = '|'))~ "Subsq_info",
        (Collection.Days.from.Index >= Days.to.Treatment.Start & Collection.Days.from.Index <= Days.to.Treatment.End) & stringr::str_detect(Therapeutic.Agents, paste0(ici_agents, collapse = '|')) ~ "ICI",
        (Collection.Days.from.Index >= Days.to.Treatment.Start & Collection.Days.from.Index <= Days.to.Treatment.End) & !stringr::str_detect(Therapeutic.Agents, paste0(ici_agents, collapse = '|')) ~ "Non_ICI_info",
        Collection.Days.from.Index > Days.to.Treatment.End & stringr::str_detect(Therapeutic.Agents, paste0(ici_agents, collapse = '|')) ~ "Prior_ICI_info",
        Collection.Days.from.Index > Days.to.Treatment.End & !stringr::str_detect(Therapeutic.Agents, paste0(ici_agents, collapse = '|')) ~ "Prior_info"
      )
    ) %>%
    dplyr::select(
      HTAN.Biospecimen.ID,
      HTAN.Parent.ID,
      Therapeutic.Agents,
      flag
    ) %>%
    dplyr::group_by(HTAN.Parent.ID, HTAN.Biospecimen.ID, flag) %>%
    dplyr::mutate(
      Therapeutic.Agents = ifelse(nchar(Therapeutic.Agents) == 0, NA_character_, Therapeutic.Agents)
    ) %>%
    dplyr::summarise(
      all_drugs = paste0(sort(unique(Therapeutic.Agents)), collapse = ", "),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = flag,
      values_from = all_drugs
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      "Sample_Treatment" = dplyr::case_when(
        !is.na(ICI)  ~ "on_sample_treatment",
        is.na(ICI) & is.na(Prior_ICI_info) ~ "pre_sample_treatment",
        is.na(ICI) & !is.na(Prior_ICI_info) ~ "post_sample_treatment"
      ),
      "Non_ICI_info" = dplyr::case_when(
        is.na(ICI) ~ Non_ICI_info,
        !is.na(ICI) & is.na(Non_ICI_info) ~  remove_ici_drugs_from_string(ICI, ici_agents),
        !is.na(ICI) & !is.na(Non_ICI_info) ~ paste_non_ici_drug_to_string(ICI, Non_ICI_info, ici_agents)
      ),
      "Prior_info" = dplyr::case_when(
        is.na(Prior_ICI_info) ~ Prior_info,
        !is.na(Prior_ICI_info) & is.na(Prior_info) ~ remove_ici_drugs_from_string(Prior_ICI_info, ici_agents),
        !is.na(Prior_ICI_info) & !is.na(Prior_info) ~ paste_non_ici_drug_to_string(Prior_ICI_info, Prior_info, ici_agents)
      ),
      "Subsq_info" = dplyr::case_when(
        is.na(Subsq_ICI_info) ~ Subsq_info,
        !is.na(Subsq_ICI_info) & is.na(Subsq_info) ~ remove_ici_drugs_from_string(Subsq_ICI_info, ici_agents),
        !is.na(Subsq_ICI_info) & !is.na(Subsq_info) ~ paste_non_ici_drug_to_string(Subsq_ICI_info, Subsq_info, ici_agents)
      ),
      "Non_ICI_info" = ifelse(nchar(Non_ICI_info) == 0, NA_character_, Non_ICI_info),
      "Prior_info" = ifelse(nchar(Prior_info) == 0, NA_character_, Prior_info),
      "Subsq_info" = ifelse(nchar(Subsq_info) == 0, NA_character_, Subsq_info),
      "ICI" = dplyr::case_when(
        is.na(ICI) ~ ICI,
        !is.na(ICI) ~ paste0(sort(unique(unlist(stringr::str_extract_all(ICI, paste0(ici_agents, collapse = '|'))))), collapse = "_")
      ),
      "Prior_ICI_info" = dplyr::case_when(
        is.na(Prior_ICI_info) ~ Prior_ICI_info,
        !is.na(Prior_ICI_info) ~ paste0(sort(unique(unlist(stringr::str_extract_all(Prior_ICI_info, paste0(ici_agents, collapse = '|'))))), collapse = "_")
      ),
      "Subsq_ICI_info" = dplyr::case_when(
        is.na(Subsq_ICI_info) ~ Subsq_ICI_info,
        !is.na(Subsq_ICI_info) ~ paste0(sort(unique(unlist(stringr::str_extract_all(Subsq_ICI_info, paste0(ici_agents, collapse = '|'))))), collapse = "_")
      )
    )

  #check if all samples from same patient have the same treatment information - so far we hadn't have this issue, but will need to accommodate that in case this happens
  patient_therapy <- dplyr::n_distinct(dplyr::select(therapy_iatlas, -HTAN.Biospecimen.ID))

  stopifnot(patient_therapy == dplyr::n_distinct(therapy_iatlas$HTAN.Parent.ID))
  ######


  therapy_info <- therapy_iatlas %>%
    dplyr::select(ICI_info = ICI, Non_ICI_info, Prior_ICI_info, Prior_info, Subsq_ICI_info, Subsq_info) %>%
    dplyr::mutate(
      "ICI_Rx" = tolower(stringi::stri_replace_all_regex(.data$ICI_info, c(", ", " ", "\\+"), "_", vectorize_all=FALSE)),
      "Non_ICI_Rx" = paste0(tolower(stringi::stri_replace_all_regex(.data$Non_ICI_info, c(", ", " ", "\\+"), "_", vectorize_all=FALSE)), "_non_ici_rx"),
      "Prior_ICI_Rx" = paste0(tolower(stringi::stri_replace_all_regex(.data$Prior_ICI_info, c(", ", " ", "\\+"), "_", vectorize_all=FALSE)), "_prior_ici_rx"),
      "Prior_Rx" = paste0(tolower(stringi::stri_replace_all_regex(.data$Prior_info, c(", ", " ", "\\+"), "_", vectorize_all=FALSE)), "_prior_rx"),
      "Subsq_ICI_Rx"= paste0(tolower(stringi::stri_replace_all_regex(.data$Subsq_ICI_info, c(", ", " ", "\\+"), "_", vectorize_all=FALSE)), "_subsq_ici_rx"),
      "Subsq_Rx" = paste0(tolower(stringi::stri_replace_all_regex(.data$Subsq_info, c(", ", " ", "\\+"), "_", vectorize_all=FALSE)), "_subsq_rx"),
    ) %>%
    dplyr::mutate( #deal with NAs
      "ICI_Rx" = ifelse(is.na(ICI_info), "na_ici_rx", ICI_Rx),
      "Non_ICI_Rx" = ifelse(is.na(Non_ICI_info), "na_non_ici_rx", Non_ICI_Rx),
      "Prior_ICI_Rx" = ifelse(is.na(Prior_ICI_info), "na_prior_ici_rx", Prior_ICI_Rx),
      "Prior_Rx" = ifelse(is.na(Prior_info), "na_prior_rx", Prior_Rx),
      "Subsq_ICI_Rx" = ifelse(is.na(Subsq_ICI_info), "na_subsq_ici_rx", Subsq_ICI_Rx),
      "Subsq_Rx" = ifelse(is.na(Subsq_info), "na_subsq_rx", Subsq_Rx)
    )


  therapy_labels <- therapy_info %>%
    dplyr::mutate( #deal with NAs
      "ICI_info" = ifelse(is.na(ICI_info), "Not available", ICI_info),
      "Non_ICI_info" = ifelse(is.na(Non_ICI_info), "Not available", Non_ICI_info),
      "Prior_ICI_info" = ifelse(is.na(Prior_ICI_info), "Not available", Prior_ICI_info),
      "Prior_info" = ifelse(is.na(Prior_info), "Not available", Prior_info),
      "Subsq_ICI_info" = ifelse(is.na(Subsq_ICI_info), "Not available", Subsq_ICI_info),
      "Subsq_info" = ifelse(is.na(Subsq_info), "Not available", Subsq_info)
    ) %>%
    dplyr::rename_with(.cols=ends_with('_info'), ~ gsub("_info", "_Rx-short_display", .x)) %>%
    dplyr::rename_with(.cols=!ends_with('-short_display'), ~ paste0(.x, "-name")) %>%
    tidyr::pivot_longer(dplyr::everything(),
                        names_to = c("parent_tag", ".value"),
                        names_sep="-") %>%
    dplyr::distinct()

  therapy_iatlas <- therapy_iatlas %>%
    dplyr::bind_cols(
      dplyr::select(therapy_info, ICI_Rx, Prior_ICI_Rx, Non_ICI_Rx, Prior_Rx, Subsq_ICI_Rx, Subsq_Rx)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      "ICI_Pathway" = dplyr::case_when(
        ICI_Rx == "na_ici_rx" ~ "none_ici_pathway",
        ICI_Rx %in% ici_agents ~ paste0(ici_pathways[ICI_Rx], "_ici_pathway"),
        !is.na(ICI_Rx) & !ICI_Rx %in% ici_agents ~ paste(paste0(sort(unique(ici_pathways[stringr::str_split(ICI_Rx, "_", simplify = TRUE)])), collapse = "_"), "_ici_pathway")
      ),
      "ICI_Target" = dplyr::case_when(
        ICI_Rx == "na_ici_rx" ~ "none_ICI_Target",
        ICI_Rx %in% ici_agents ~ paste0(ici_targets[ICI_Rx], "_ici_target"),
        !is.na(ICI_Rx) & !ICI_Rx %in% ici_agents ~ paste(paste0(sort(unique(ici_targets[stringr::str_split(ICI_Rx, "_", simplify = TRUE)])), collapse = "_"), "_ici_target")
      )
    ) %>%
    dplyr::select(
      HTAN.Biospecimen.ID,
      Sample_Treatment,
      ICI_Rx,
      ICI_Pathway,
      ICI_Target,
      Prior_ICI_Rx,
      Subsq_ICI_Rx,
      Non_ICI_Rx,
      Prior_Rx,
      Subsq_Rx
    )


  #3. CONSOLIDATING

  htan_tags <- msk_df[["subtype"]] %>%
    dplyr::select(-Cell) %>%
    dplyr::distinct() %>%
    dplyr::right_join(msk_df[["biospecimen"]],  by = c("HTAN_Biospecimen_ID" = "HTAN.Biospecimen.ID")) %>%
    dplyr::right_join(dplyr::select(msk_df[["therapy"]], "HTAN.Participant.ID",  "Treatment.Intent.Type"),  by = c("HTAN.Parent.ID" = "HTAN.Participant.ID"), relationship = "many-to-many") %>%
    dplyr::select(
      "HTAN.Biospecimen.ID" = "HTAN_Biospecimen_ID",
      "HTAN.Parent.ID",
      "Preservation.Method",
      "Collection.Days.from.Index",
      "Tumor.Tissue.Type",
      "Site.of.Resection.or.Biopsy",
      "subtype",
      "Treatment.Intent.Type"
    ) %>%
    dplyr::inner_join(
      dplyr::select(msk_df[["diagnosis"]], -Site.of.Resection.or.Biopsy), #removing duplicated information from biospecimen df
      by = dplyr::join_by("HTAN.Parent.ID" == "HTAN.Participant.ID")
    ) %>%
    dplyr::inner_join(
      therapy_iatlas,
      by = "HTAN.Biospecimen.ID"
    ) %>%
    dplyr::mutate(
      "Biopsy_Site_info" = format_entry(.data$Site.of.Resection.or.Biopsy, c("Upper lobe ", "Intrathoracic ", " NOS", "s of head face and neck", "Lower lobe ", "Middle lobe ", "s of axilla or arm"), add_underscore = FALSE),
      "Biopsy_Site" = paste0(format_entry(Biopsy_Site_info, "_", add_underscore = TRUE), "_biopsy_site"),
      "Cancer_Tissue_info" = format_entry(.data$Tissue.or.Organ.of.Origin, (" NOS")),
      "Cancer_Tissue" = paste0(format_entry(.data$Tissue.or.Organ.of.Origin, (" NOS"), add_underscore = TRUE), "_cancer_tissue"),
      "Response" = "na_response",
      "Response_info" = "Not available",
      "Responder" = dplyr::case_when(
        Progression.or.Recurrence == "Yes - Progression or Recurrence"  ~ "false_responder",
        Progression.or.Recurrence == "No"  ~ "true_responder",
        Progression.or.Recurrence == "unknown"  ~ "na_responder",
        Progression.or.Recurrence == "Not Reported"  ~ "na_responder",
        is.na(Progression.or.Recurrence) ~ "na_responder"
      ),
      "Progression" = dplyr::case_when(
        Progression.or.Recurrence == "Yes - Progression or Recurrence" ~ "true_progression",
        Progression.or.Recurrence == "No"  ~ "false_progression",
        Progression.or.Recurrence == "unknown"  ~ "na_progression",
        Progression.or.Recurrence == "Not Reported"  ~ "na_progression",
        is.na(Progression.or.Recurrence) ~ "na_progression"
      ),
      "Clinical_Benefit" = dplyr::case_when(
        Progression.or.Recurrence == "Yes - Progression or Recurrence"  ~ "false_clinical_benefit",
        Progression.or.Recurrence == "No"  ~ "true_clinical_benefit",
        Progression.or.Recurrence == "unknown"  ~ "na_clinical_benefit",
        Progression.or.Recurrence == "Not Reported"  ~ "na_clinical_benefit",
        is.na(Progression.or.Recurrence) ~ "na_clinical_benefit"
      ),
      "FFPE" = dplyr::case_when(
        .data$Preservation.Method == "Formalin fixed paraffin embedded - FFPE" ~ "true_ffpe",
        .data$Preservation.Method %in% c("", "Not Reported") ~ "na_ffpe",
        TRUE ~ "false_ffpe"
      ),
      "Metastasized" = dplyr::case_when(
        .data$Tumor.Tissue.Type == "Metastatic" ~ "true_metastasized",
        .data$Tumor.Tissue.Type == "Primary" ~ "false_metastasized",
        .data$Tumor.Tissue.Type == "Recurrent" ~ "false_metastasized",
        .data$Tumor.Tissue.Type == "Local recurrence" ~ "false_metastasized",
        .data$Tumor.Tissue.Type == "Not Otherwise Specified" ~ "na_metastasized",
        .data$Tumor.Tissue.Type == "" ~ "na_metastasized",
      ),
      "Clinical_Stage" = dplyr::if_else(
        is.na(.data$`AJCC.Clinical.Stage`),
        "na_clinical_stage",
        .data$`AJCC.Clinical.Stage`
      ),
      "TCGA_Study" = "SCLC",
      "TCGA_Subtype" = dplyr::case_when( # Still using this tag name, maybe consider changing to something more generic
        !is.na(subtype) ~ subtype,
        is.na(subtype) ~ "na_tcga_subtype"
      ),
      "TCGA_Subtype_info" = dplyr::if_else(
        TCGA_Subtype == "na_tcga_subtype",
        "Not available",
        TCGA_Subtype
      ),
      "Polyp_Histology_info" = "Not Available",
      "Polyp_Histology" = "na_polyp_histology", #annotation added for Vanderbilt
      "Tumor_tissue_type_info" = dplyr::case_when( #ATTENTION! This is the same column used to generate the Metastasized annotation, but for Vanderbilt this annotation is key to group samples
        .data$Tumor.Tissue.Type == "Not Otherwise Specified" ~ "Not Available",
        TRUE ~ .data$Tumor.Tissue.Type
      ),
      "Tumor_tissue_type" = dplyr::case_when( #ATTENTION! This is the same column used to generate the Metastasized annotation
        .data$Tumor.Tissue.Type == "Not Otherwise Specified" ~ "na_tumor_tissue_type",
        TRUE ~ paste0(tolower(gsub(" - ", "_", .data$Tumor.Tissue.Type)), "_tumor_tissue_type")
      ),
      "NeoICI_Rx_info" = dplyr::if_else(
        Prior_ICI_Rx != "na_prior_ici_rx" & Treatment.Intent.Type == "Neoadjuvant",
        Prior_ICI_Rx,
        "none_neoici_rx"
      ),
      "NeoICI_Rx" = dplyr::if_else(
        Prior_ICI_Rx != "na_prior_ici_rx" & Treatment.Intent.Type == "Neoadjuvant",
        gsub("prior_ici_rx","neoici_rx", Prior_ICI_Rx),
        "none_neoici_rx"
      ),
      "Tissue_Subtype" = "na_tissue_subtype"
    )

  columns_with_labels <-  colnames(dplyr::select(htan_tags, dplyr::ends_with("info")))
  columns_with_names <- gsub("_info", "", columns_with_labels)

  tags_info <- htan_tags %>%
    dplyr::select(dplyr::all_of(columns_with_labels),
                  dplyr::all_of(columns_with_names)) %>%
    dplyr::rename_with(.cols=ends_with('_info'), ~ gsub("_info", "-short_display", .x)) %>%
    dplyr::rename_with(.cols=!ends_with('-short_display'), ~ paste0(.x, "-name")) %>%
    tidyr::pivot_longer(dplyr::everything(),
                        names_to = c("parent_tag", ".value"),
                        names_sep="-") %>%
    dplyr::filter(!is.na(short_display)) %>%
    dplyr::distinct() %>%
    rbind(therapy_labels)


  htan_tags <- htan_tags %>%
    dplyr::select(
      HTAN.Biospecimen.ID,
      HTAN.Parent.ID,
      Sample_Treatment,
      Biopsy_Site,
      Cancer_Tissue,
      Tissue_Subtype,
      Tumor_tissue_type,
      Polyp_Histology,
      Response,
      Responder,
      Progression,
      Clinical_Benefit,
      FFPE,
      Metastasized,
      Clinical_Stage,
      TCGA_Study,
      TCGA_Subtype,
      NeoICI_Rx,
      ICI_Rx,
      ICI_Pathway,
      ICI_Target,
      Prior_ICI_Rx,
      Subsq_ICI_Rx,
      Non_ICI_Rx,
      Prior_Rx,
      Subsq_Rx
    )

  readr::write_csv(htan_tags, "htan_msk_tags_wide.csv", na = "")
  file_entity <- synapseclient$File("htan_msk_tags_wide.csv", parent = "syn52570141")
  syn$store(file_entity)

  #writing key short labels
  readr::write_csv(tags_info, "htan_msk_labels.csv", na = "")
  file_entity <- synapseclient$File("htan_msk_labels.csv", parent = "syn52570141")
  syn$store(file_entity)



 ###### Vanderbilt
  #1. LOADING DATA

  vanderbilt_files <- c("biospecimen"="syn38868462",
                        "demographics" ="syn38868669",
                        "diagnosis" = "syn39051142",
                        "followup" = "syn39051632",
                        "therapy" = "syn39051860"
  )

  files_path <- paste("inst/", vanderbilt_files, ".csv", sep = "") #locally stored, ideally will change to read directly from synapse
  vanderbilt_df <-  purrr::map(files_path, read.csv)
  names(vanderbilt_df) <- names(vanderbilt_files)

  #2. ADJUSTING TREATMENT INFORMATION

  #This dataset has NO data for treatment, so we will manually add the treatment info in iAtlas format

  therapy_iatlas <- vanderbilt_df$biospecimen %>%
    dplyr::select(
      "HTAN.Biospecimen.ID"
    ) %>%
    dplyr::mutate(
      "Sample_Treatment" = "pre_sample_treatment",
      "ICI_Rx" = "na_ici_rx",
      "Non_ICI_Rx" = "na_non_ici_rx",
      "Prior_ICI_Rx" =  "na_prior_ici_rx",
      "Prior_Rx" = "na_prior_rx",
      "Subsq_ICI_Rx" = "na_subsq_ici_rx",
      "Subsq_Rx" = "na_subsq_rx",
      "ICI_Pathway" = "none_ici_pathway",
      "ICI_Target" = "none_ICI_Target"
    )


  #3. CONSOLIDATING

  htan_tags <- vanderbilt_df[["biospecimen"]]%>%
    dplyr::select(
      "HTAN.Biospecimen.ID",
      "HTAN.Parent.ID",
      "Preservation.Method",
      "Collection.Days.from.Index",
      "Tumor.Tissue.Type",
      "Site.of.Resection.or.Biopsy",
      "Polyp.Histology"
    ) %>%
    dplyr::inner_join(
      dplyr::select(vanderbilt_df[["diagnosis"]], -Site.of.Resection.or.Biopsy), #removing duplicated information from biospecimen df
      by = dplyr::join_by("HTAN.Parent.ID" == "HTAN.Participant.ID")
    ) %>%
    dplyr::inner_join(
      therapy_iatlas,
      by = "HTAN.Biospecimen.ID"
    ) %>%
    dplyr::mutate(
      "Biopsy_Site_info" = dplyr::case_when(
        .data$Site.of.Resection.or.Biopsy == "Not Reported" ~ "Not available",
        TRUE ~ format_entry(.data$Site.of.Resection.or.Biopsy, c("Upper lobe ", "Intrathoracic ", " NOS", "s of head face and neck", "Lower lobe ", "Middle lobe ", "s of axilla or arm"), add_underscore = FALSE)),
      "Biopsy_Site" = dplyr::case_when(
        .data$Biopsy_Site_info == "Not available" ~ "na_biopsy_site",
        TRUE ~ paste0(format_entry(Biopsy_Site_info, "_", add_underscore = TRUE), "_biopsy_site")),
      "Cancer_Tissue_info" = dplyr::case_when(
        .data$Tissue.or.Organ.of.Origin == "Not Reported" ~ "Not available",
        TRUE ~ format_entry(.data$Tissue.or.Organ.of.Origin, (" NOS"))),
      "Cancer_Tissue" = dplyr::case_when(
        .data$Tissue.or.Organ.of.Origin == "Not Reported" ~ "na_cancer_tissue",
        TRUE ~ paste0(format_entry(.data$Tissue.or.Organ.of.Origin, (" NOS"), add_underscore = TRUE), "_cancer_tissue")),
      "Response" = "na_response",
      "Response_info" = "Not available",
      "Responder" = dplyr::case_when(
        Progression.or.Recurrence == "Yes - Progression or Recurrence"  ~ "false_responder",
        Progression.or.Recurrence == "No"  ~ "true_responder",
        Progression.or.Recurrence == "unknown"  ~ "na_responder",
        Progression.or.Recurrence == "Not Reported"  ~ "na_responder",
        is.na(Progression.or.Recurrence) ~ "na_responder"
      ),
      "Progression" = dplyr::case_when(
        Progression.or.Recurrence == "Yes - Progression or Recurrence" ~ "true_progression",
        Progression.or.Recurrence == "No"  ~ "false_progression",
        Progression.or.Recurrence == "unknown"  ~ "na_progression",
        Progression.or.Recurrence == "Not Reported"  ~ "na_progression",
        is.na(Progression.or.Recurrence) ~ "na_progression"
      ),
      "Clinical_Benefit" = dplyr::case_when(
        Progression.or.Recurrence == "Yes - Progression or Recurrence"  ~ "false_clinical_benefit",
        Progression.or.Recurrence == "No"  ~ "true_clinical_benefit",
        Progression.or.Recurrence == "unknown"  ~ "na_clinical_benefit",
        Progression.or.Recurrence == "Not Reported"  ~ "na_clinical_benefit",
        is.na(Progression.or.Recurrence) ~ "na_clinical_benefit"
      ),
      "FFPE" = dplyr::case_when(
        .data$Preservation.Method == "Formalin fixed paraffin embedded - FFPE" ~ "true_ffpe",
        .data$Preservation.Method %in% c("", "Not Reported") ~ "na_ffpe",
        TRUE ~ "false_ffpe"
      ),
      "Metastasized" = dplyr::case_when(
        .data$Tumor.Tissue.Type == "Metastatic" ~ "true_metastasized",
        .data$Tumor.Tissue.Type == "Primary" ~ "false_metastasized",
        .data$Tumor.Tissue.Type == "Recurrent" ~ "false_metastasized",
        .data$Tumor.Tissue.Type == "Local recurrence" ~ "false_metastasized",
        .data$Tumor.Tissue.Type %in% c("Premalignant","Normal", "Atypia - hyperplasia") ~ "false_metastasized",
        .data$Tumor.Tissue.Type == "Not Otherwise Specified" ~ "na_metastasized",
        .data$Tumor.Tissue.Type == "" ~ "na_metastasized",
      ),
      "Clinical_Stage" = dplyr::case_when(
        is.na(.data$`AJCC.Clinical.Stage`) ~ "na_clinical_stage",
        .data$`AJCC.Clinical.Stage` == "Not Reported" ~ "na_clinical_stage",
        TRUE ~ .data$`AJCC.Clinical.Stage`
      ),
      "TCGA_Study" = "na_tcga_study",
      "TCGA_Study_info" = "Not available",
      "TCGA_Subtype" = "na_tcga_subtype",
      "TCGA_Subtype_info" = "Not available",
      "NeoICI_Rx" = "none_neoici_rx",
      "Tissue_Subtype" = "na_tissue_subtype",
      "Polyp_Histology_info" = dplyr::case_when(
        .data$Polyp.Histology == "" ~ "Not Available",
        TRUE ~ .data$Polyp.Histology
      ),
      "Polyp_Histology" = dplyr::case_when(
        .data$Polyp.Histology == "" ~ "na_polyp_histology",
        .data$Polyp.Histology == "Unknown" ~ "unknown_polyp_histology",
        TRUE ~ paste0(tolower(gsub(" - ", "_", .data$Polyp.Histology)), "_polyp_histology"),
      ),
      "Tumor_tissue_type_info" = dplyr::case_when( #ATTENTION! This is the same column used to generate the Metastasized annotation, but for Vanderbilt this annotation is key to group samples
        .data$Tumor.Tissue.Type == "Not Otherwise Specified" ~ "Not Available",
        TRUE ~ .data$Tumor.Tissue.Type
      ),
      "Tumor_tissue_type" = dplyr::case_when( #ATTENTION! This is the same column used to generate the Metastasized annotation
        .data$Tumor.Tissue.Type == "Not Otherwise Specified" ~ "na_tumor_tissue_type",
        TRUE ~ paste0(tolower(gsub(" - ", "_", .data$Tumor.Tissue.Type)), "_tumor_tissue_type")
      )
    )

  columns_with_labels <-  colnames(dplyr::select(htan_tags, dplyr::ends_with("info")))
  columns_with_names <- gsub("_info", "", columns_with_labels)

  tags_info <- htan_tags %>%
    dplyr::select(dplyr::all_of(columns_with_labels),
                  dplyr::all_of(columns_with_names)) %>%
    dplyr::rename_with(.cols=ends_with('_info'), ~ gsub("_info", "-short_display", .x)) %>%
    dplyr::rename_with(.cols=!ends_with('-short_display'), ~ paste0(.x, "-name")) %>%
    tidyr::pivot_longer(dplyr::everything(),
                        names_to = c("parent_tag", ".value"),
                        names_sep="-") %>%
    dplyr::filter(!is.na(short_display)) %>%
    dplyr::distinct()


  htan_tags <- htan_tags %>%
    dplyr::select(
      HTAN.Biospecimen.ID,
      HTAN.Parent.ID,
      Sample_Treatment,
      Biopsy_Site,
      Cancer_Tissue,
      Tissue_Subtype,
      Tumor_tissue_type,
      Polyp_Histology,
      Response,
      Responder,
      Progression,
      Clinical_Benefit,
      FFPE,
      Metastasized,
      Clinical_Stage,
      TCGA_Study,
      TCGA_Subtype,
      NeoICI_Rx,
      ICI_Rx,
      ICI_Pathway,
      ICI_Target,
      Prior_ICI_Rx,
      Subsq_ICI_Rx,
      Non_ICI_Rx,
      Prior_Rx,
      Subsq_Rx
    )

  readr::write_csv(htan_tags, "htan_vanderbilt_tags_wide.csv", na = "")
  file_entity <- synapseclient$File("htan_vanderbilt_tags_wide.csv", parent = "syn52570141")
  syn$store(file_entity)

  #writing key short labels
  readr::write_csv(tags_info, "htan_vanderbilt_labels.csv", na = "")
  file_entity <- synapseclient$File("htan_vanderbilt_labels.csv", parent = "syn52570141")
  syn$store(file_entity)

}

