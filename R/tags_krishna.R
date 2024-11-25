tags_shiao <- function() {

  require(magrittr)
  require(iatlasGraphQLClient)
  require(rlang)
  require(Polychrome)
  syn <- create_synapse_login()

  clinical_df <- syn$get("syn59202673") %>%
    purrr::pluck("path") %>%
    openxlsx::read.xlsx(., sheet = 1) %>%
    dplyr::as_tibble()

  #1. ADDING NEW PARENT GROUP
  # We are not adding any new parent group

  #2. ADDINNG NEW GROUPS
  # There are few new tags to add for this dataset, so we will do this manually:
  full_tags <- data.frame(
    name = c("nivolumab_neoici_rx", "nivolumab_prior_ici_rx", "sunitinib_ipilimumab_nivolumab_prior_ici_rx", "RCC", "RCC_ccRCC"),
    short_display = c("Nivolumab", "Nivolumab", "Sunitinib, Ipilimumab, Nivolumab", "RCC", "ccRCC"),
    long_display= c("Nivolumab", "Nivolumab", "Sunitinib, Ipilimumab, Nivolumab", "Renal cell carcinomas", "Clear cell renal cell carcinomas"),
    color= c("#DBD56E",
             "#DBD56E",
             "#E2D5ED",
             "#009444",
             NA_character_
    ),
    description = c("Nivolumab administered in neoadjuvant setting",
                    "Patient received nivolumab prior to ICI study",
                    "Patient received sunitinib, ipilimumab, nivolumab prior to ICI study",
                    "Renal cell carcinomas",
                    NA_character_),
    tag_type = "group",
    order = c(4,
              7,
              8,
              NA_integer_,
              NA_integer_
    )
  ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "tags"
    )

  synapse_store_table_as_csv(
    syn,
    full_tags,
    "syn59210601",
    "tags"
  )


}
