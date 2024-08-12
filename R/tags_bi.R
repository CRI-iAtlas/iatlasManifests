tags_bi <- function() {

  require(magrittr)
  require(iatlasGraphQLClient)
  require(rlang)
  syn <- create_synapse_login()

  #1. ADDING NEW PARENT GROUP
  # We are not adding any new parent group

  #2. ADDING NEW GROUPS
  # There are few new tags to add for this dataset, so we will do this manually:
  tags <- data.frame(
    name = c("vegf_tki_unnamed_non_ici_rx", "unnamed_ICI_Rx"),
    short_display = c("Unnamed VEGF TKIs treatment", "Unnamed ICI drug"),
    long_display= c("Unnamed VEGF tyrosine kinase inhibitors treatment", "Normal Adjacent"),
    color= c("#c8decc",
             "#FFD700"
    ),
    description = c("ICI therapy combined with VEGF tyrosine kinase inhibitor treatment",
                    "Name of ICI drug not provided"),
    tag_type = "group",
    order = c(30,
              8
    )
  ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    tags,
    "syn60530046",
    "tags"
  )


}
