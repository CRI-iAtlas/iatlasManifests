tags_amadeus <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  #1. ADDING NEW PARENT GROUP
  # We will add a new parent group for the AMADEUS cancer type annotation

  parent_tags<- dplyr::tibble("name"= character(),
                              "short_display"= character(),
                              "long_display"= character(),
                              "color"= character(),
                              "description"= character(),
                              "tag_type"= character(),
                              "order"= integer()) %>%
    dplyr::add_row("name"= "AMADEUS_Study",
                   "short_display"= "AMADEUS tumor type",
                   "long_display"= "Tumor type described in the publication for the AMADEUS trial",
                   "color"= NA_character_,
                   "description"= "Tumor type listed in the publication for the AMADEUS trial (Tsimberidou et al, 2024)",
                   "tag_type"= "parent_group",
                   "order"= 25)

  #2. ADDING NEW GROUPS
  # There are few new tags to add for this dataset, so we will do this manually:
  amadeus_tags <- data.frame(
    name = character(),
    short_display = character(),
    long_display= character(),
    color= character(),
    description = character(),
    tag_type = character(),
    order = numeric()
  )
  #add tags for new cancer tissues
  amadeus_tags[1,] <- c("uterus_cancer_tissue","Uterus","Uterus", "#8DD3C7", "Uterus is the cancer tissue", "group", NA_integer_)
  amadeus_tags <- rbind(amadeus_tags, c("colon_rectum_cancer_tissue", "Colon rectum","Colon rectum", "#FB8072", "Colon rectum is the cancer tissue", "group", NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c("liver_cancer_tissue", "Liver", "Liver", "#80B1D3", "Liver is the cancer tissue", "group", NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c("thyroid_cancer_tissue", "Thyroid", "Thyroid", "#FDB462", "Thyroid is the cancer tissue", "group", NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c("pelvis_cancer_tissue", "Pelvis", "Pelvis", "#B3DE69", "Pelvis is the cancer tissue", "group", NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c("peritoneum_cancer_tissue", "Peritoneum", "Peritoneum", "#FCCDE5", "Peritoneum is the cancer tissue", "group", NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c("neuroendocrine_cancer_tissue", "Neuroendocrine",  "Neuroendocrine", "#BC80BD",  "Neuroendocrine is the cancer tissue", "group", NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c("duodenum_cancer_tissue", "Duodenum", "Duodenum", "#CCEBC5", "Duodenum is the cancer tissue", "group", NA_integer_))


  #add cancer types acronyms from AMADEUS
  amadeus_tags <- rbind(amadeus_tags, c("BRCA_amadeus", "BRCA", "Breast invasive carcinoma", "#ED2891", NA_character_, "group", NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c('CRCA_amadeus', 'CRCA', 'Colorectal', "#1B9E77",NA_character_, 'group', NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c('GSCA_amadeus', 'GSCA', 'gastric', "#D95F02" , NA_character_, 'group', NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c('GEJC_amadeus', 'GEJC', 'gastroesophageal',"#7570B3", NA_character_, 'group', NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c('HNCA_amadeus', 'HNCA', 'Head and Neck', "#66A61E", NA_character_, 'group', NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c('HCCA_amadeus', 'HCCA', 'hepatocellular carcinoma',"#E6AB02", NA_character_, 'group', NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c('HECH_amadeus', 'HECH', 'Hepatocellular cholangiocarcinoma', "#A6761D", NA_character_, 'group', NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c('LUCA_amadeus', 'LUCA', 'Lung',"#666666", NA_character_, 'group', NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c('NESK_amadeus', 'NESK', 'Merkel cell', "#7FC97F", NA_character_, 'group', NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c('NEUC_amadeus', 'NEUC', 'Neuroendocrine', "#BEAED4", NA_character_, 'group', NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c('LUCA_amadeus', 'LUCA', 'non-small cell lung', "#FDC086", NA_character_, 'group', NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c('PANC_amadeus', 'PANC', 'pancreatic', "#FFFF99", NA_character_, 'group', NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c('AMPV_amadeus', 'AMPV', 'papilla of vater', "#386CB0", NA_character_, 'group', NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c('PELV_amadeus', 'PELV', 'pelvic', "#F0027F", NA_character_, 'group', NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c('PRTC_amadeus', 'PRTC', 'peritoneal', "#BF5B17", NA_character_, 'group', NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c('CRPC_amadeus', 'CRPC', 'castration resistant prostate cancer', "#FFB74D", NA_character_, 'group', NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c('SARC_amadeus', 'SARC', 'Sarcoma', "#00A99D", NA_character_, 'group', NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c('THYR_amadeus', 'THYR', 'Thyroid',"#E41A1C", NA_character_, 'group', NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c('UTCA_amadeus', 'UTCA', 'Uterine', "#984EA3", NA_character_, 'group', NA_integer_))

  #TODO: still need to add new TCGA cancer types, if necessary
  #add new cancer types acronyms
  #amadeus_tags <- rbind(amadeus_tags, c("NESK", "NESK", "", "#CCEBC5", "Duodenum is the cancer tissue", "group", NA_integer_))

  amadeus_tags <- amadeus_tags %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    amadeus_tags,
    "",
    "tags"
  )


}
