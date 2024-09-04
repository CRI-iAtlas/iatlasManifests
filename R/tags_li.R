tags_li <- function() {

  require(magrittr)
  require(iatlasGraphQLClient)
  require(rlang)
  require(Polychrome)
  syn <- create_synapse_login()

  #1. ADDING NEW PARENT GROUP
  # We are not adding any new parent group

  #2. ADDINNG NEW GROUPS
  # There are few new tags to add for this dataset, so we will do this manually:
  li_tags <- data.frame(
    name = c("i_clinical_stage", "normal_adjacent_tumor_tissue_type", "thrombus_biopsy_site"),
    short_display = c("I", "Normal Adjacent", "Thrombus"),
    long_display= c("I", "Normal Adjacent", "Thrombus"),
    color= c("#c8decc",
             "#FFD700",
             "#800000"
    ),
    description = c("Patient at clinical stage I",
                    "Type of tumor tissue collected is Normal Adjacent",
                    "Samples collected from Thrombus"),
    tag_type = "group",
    order = c(0,
              11,
              24
    )
  ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )


  synapse_store_table_as_csv(
    syn,
    li_tags,
    "syn60157437",
    "tags"
  )


}
