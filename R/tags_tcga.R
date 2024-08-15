tags_tcga <- function() {

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  tags <-
    synapse_feather_id_to_tbl(syn, "syn23545011") %>%
    dplyr::select(
      "name",
      "short_display",
      "long_display",
      "color",
      "description" = "characteristics",
      "tag_type" = "type"
    ) %>%
    dplyr::arrange(.data$name) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "tags"
    )

  # we had to add the patient data in tags tables, so it can be retrieved by queries:
  new_tags <- data.frame(
    "name" = c("gender", "female", "male",
               "race", "white_race", "black_or_african_american_race",
               "asian_race", "native_hawaiian_or_other_pacific_islander_race", "american_indian_or_alaska_native_race",
               "ethnicity", "na_ethnicity", "not_hispanic_or_latino_ethnicity", "hispanic_or_latino_ethnicity" ),
    "short_display" = c("gender", "Female", "Male",
                        "race", "White", "Black or African American",
                        "Asian", "Native Hawaiian or other Pacific Islander", "American Indian or Alaska native",
                        "ethnicity", "Not available",  "not Hispanic or Latino", "Hispanic or Latino"),
    "long_display"= c("gender", "Female", "Male",
                      "race", "White", "Black or African American",
                      "Asian", "Native Hawaiian or other Pacific Islander", "American Indian or Alaska native",
                      "ethnicity", "Not available",  "not Hispanic or Latino", "Hispanic or Latino"),
    "color" = c(NA_character_, "#D41159", "#170ec4",
                NA_character_, "#377eb8", "#ff7f00", "#4daf4a", "#984ea3", "#e41a1c",
                NA_character_, "#868A88", "#377eb8", "#ff7f00"),
    "description"= c("Patient gender" , "Patient is female", "Patient is male",
                     "Race of the patient", "Race of the patient is White", "Race of the patient is Black or African American",
                     "Race of the patient is Asian", "Race of the patient is Native Hawaiian or other Pacific Islander", "Race of the patient is American Indian or Alaska native",
                     "Ethnicity of the patient", "Ethnicity of the patient is not available",  "Patients is not Hispanic or Latino", "Patients is Hispanic or Latino"),
    "tag_type" = c("parent_group","group", "group", "parent_group", "group", "group", "group", "group", "group", "parent_group", "group", "group", "group")
  ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "tags"
    )

  synapse_store_table_as_csv(
    syn,
    rbind(samples_to_tags, new_tags),
    "syn51080175",
    "tags"
  )



}
