tags_htan_msk <- function() {

  require(magrittr)
  require(iatlasGraphQLClient)
  require(rlang)
  require(Polychrome)
  syn <- create_synapse_login()


  shiao_obs <- synapse_csv_id_to_tbl(syn, "syn55273095")

  #1. ADDING NEW PARENT GROUP
  # We are not adding any new parent group

  #2. ADDINNG NEW GROUPS
  # There are few new tags to add for this dataset, so we will do this manually:
  shiao_tags <- data.frame(
    name = c("breast_cancer_tissue", "breast_biopsy_site", "ii_clinical_stage", "BRCA_TNBC"),
    short_display = c("Breast", "Breast", "II", "TNBC"),
    long_display= c("Breast", "Breast", "II", "Triple-negative breast cancer"),
    color= c("#FFB74D",
             "#004D40",
             "#E2D5ED",
             NA_character_
    ),
    description = c("Breast is the cancer tissue",
                    "Samples collected from Breast",
                    "Patient at clinical stage II",
                    NA_character_),
    tag_type = "group",
    order = c(9,
              22,
              0,
              NA_integer_
    )
  )

  # However, the treatment post ICI will require some extra work
  subsq_rx_info <- synapse_csv_id_to_tbl(syn, "syn58895700") %>%
    dplyr::select(name = names, short_display) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      long_display = short_display,
      color = RColorBrewer::brewer.pal(8, "Set3"),
      description = paste("Patient had subsequent treatment with",
                          gsub("\\+", ", ", short_display)),
      tag_type = "group",
      order = 34:41
    )



  full_tags <- shiao_tags %>%
    dplyr::bind_rows(subsq_rx_info) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "tags"
    )

  synapse_store_table_as_csv(
    syn,
    full_tags,
    "syn58896098",
    "tags"
  )


}
