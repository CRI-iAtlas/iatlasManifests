tags_porter <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  #1. ADDING NEW PARENT GROUP
  # We are not adding any new parent group

  #2. ADDINNG NEW GROUPS
  # There are few new tags to add for this dataset, so we will do this manually:
  porter_tags <- data.frame(
    name = c("CRPC",
             "CRPC_mCRPC",
             "NKTR_214_non_ici_rx",
             "SBRT_CDX_301_poly_ICLC_non_ici_rx",
             "CDX_301_INO_5151_non_ici_rx",
             "prostate_cancer_tissue"),
    short_display = c("CRPC", "CRPC_mCRPC", "NKTR-214", "SBRT + CDX-301 + poly-ICLC", "CDX-301 + INO-5151", "Prostate"),
    long_display= c("castration resistant prostate cancer", "metastatic castration resistant prostate cancer", "NKTR-214", "SBRT + CDX-301 + poly-ICLC", "CDX-301 + INO-5151", "Prostate"),
    color= c("#FFB74D",
             NA_character_,
             "#9b19f5",
             "#ffa300",
             "#dc0ab4",
             "#004D40"
    ),
    description = c(NA_character_,
                    NA_character_,
                    "ICI therapy combined with NKTR-214",
                    "ICI therapy combined with SBRT + CDX-301 + poly-ICLC",
                    "ICI therapy combined with CDX-301 + INO-5151",
                    "Prostate is the cancer tissue"
                    ),
    tag_type = "group",
    order = c(NA_integer_,
              NA_integer_,
              33,
              34,
              35,
              10
    )
  ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    tags,
    "",
    "tags"
  )


}
