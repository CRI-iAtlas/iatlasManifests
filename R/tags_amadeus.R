tags_amadeus <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  #1. ADDING NEW PARENT GROUP
  # We are not adding any new parent group

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

  amadeus_tags[1,] <- c("uterus_cancer_tissue","Uterus","Uterus", "#8DD3C7", "Uterus is the cancer tissue", "group", NA_integer_)
  amadeus_tags[2,] <- c("ureter_cancer_tissue", "Ureter", "Ureter", "#FFFFB3", "Ureter is the cancer tissue", "group", NA_integer_)
  amadeus_tags[3,] <- c("penis_cancer_tissue", "Penis", "Penis", "#BEBADA", "Penis is the cancer tissue", "group", NA_integer_)
  amadeus_tags[4,] <- c("colon_rectum_cancer_tissue", "Colon rectum","Colon rectum", "#FB8072", "Colon rectum is the cancer tissue", "group", NA_integer_)
  amadeus_tags[5,] <- c("liver_cancer_tissue", "Liver", "Liver", "#80B1D3", "Liver is the cancer tissue", "group", NA_integer_)
  amadeus_tags[6,] <- c("thyroid_cancer_tissue", "Thyroid", "Thyroid", "#FDB462", "Thyroid is the cancer tissue", "group", NA_integer_)
  amadeus_tags[7,] <- c("pelvis_cancer_tissue", "Pelvis", "Pelvis", "#B3DE69", "Pelvis is the cancer tissue", "group", NA_integer_)
  amadeus_tags[8,] <- c("peritoneum_cancer_tissue", "Peritoneum", "Peritoneum", "#FCCDE5", "Peritoneum is the cancer tissue", "group", NA_integer_)
  amadeus_tags[9,] <- c("retroperitoneum_cancer_tissue", "Retroperitoneum", "Retroperitoneum", "#D9D9D9", "Retroperitoneum is the cancer tissue", "group", NA_integer_)
  amadeus_tags[10,] <- c("neuroendocrine_cancer_tissue", "Neuroendocrine",  "Neuroendocrine", "#BC80BD",  "Neuroendocrine is the cancer tissue", "group", NA_integer_)
  amadeus_tags[11,] <- c("duodenum_cancer_tissue", "Duodenum", "Duodenum", "#CCEBC5", "Duodenum is the cancer tissue", "group", NA_integer_)

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
