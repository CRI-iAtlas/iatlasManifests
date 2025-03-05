tags_TEMPLATE <- function() {#UPDATE function name

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  # We use the terminology "tags" to refer to categorical clinical data stored - any clinical data that is numeric should be added to the features_to_samples table
  # To illustrate the concept, consider a hypothetical example where we want to record an "Attribute_A" to samples,
  # available as a table below:

  # Sample_Name | Attribute_A
  # S_123456789 | A1

  # For this example, we would need to do the following steps:
  #1. Add "Attribute_A" to the tags table (here) as a "parent_group"
  #2. Add "A1" to the tags table (here) as a "group"
  #3. Add the relationship S_123456789 - A1 to the samples_to_tags table
  #4. Add the relationship Attribute_A - A1 (ie, that A1 is a group under Attribute_A) to the tags_to_tags table

  # Step 3 is always required if we want to store categorical information for the samples
  # If a new dataset has no new "parent_group", step 1 is not necessary
  # If a a new dataset has no new "parent_group" and no new "group", step 1, 2 and 4 are not necessary
  # If a a new dataset has only new "group", steps  2, 3 and 4 are necessary

  # The tags table has the following columns:
  # "name"= name of the feature, with no spaces, lower cases. Needs to be unique. For a "group", append the "parent_group" at the end (in the example above "A1" would become "a1_attribute_a")
  # "short_display"= the name that will be displayed at the app in plots
  # "long_display"= the name that will be displayed at the app in menus. Can be the same as short_display
  # "color"= color to be used for this group in plots
  # "description"= description to be used in plots and texts
  # "tag_type"= "parent_group" or "group"(see example above for an explanation for when to use each),
  # "order"= can be set to NA, use a number if you want to set an order of groups

  #1. ADDING NEW PARENT GROUP
  # We will add a new parent group manually

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
  # There are few new tags to add for this dataset, so we add some of them manually:
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

  #add cancer types acronyms from AMADEUS
  amadeus_tags <- rbind(amadeus_tags, c("BRCA_amadeus", "BRCA", "Breast invasive carcinoma", "#ED2891", NA_character_, "group", NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c('CRCA_amadeus', 'CRCA', 'Colorectal', "#1B9E77",NA_character_, 'group', NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c('GSCA_amadeus', 'GSCA', 'gastric', "#D95F02" , NA_character_, 'group', NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c('GEJC_amadeus', 'GEJC', 'gastroesophageal',"#7570B3", NA_character_, 'group', NA_integer_))

  #add tags for new prior treatment with ICI
  amadeus_tags <- rbind(amadeus_tags, c('nivolumab_vopratelimab_prior_ici_rx', 'nivolumab, vopratelimab', 'Nivolumab, Vopratelimab', "#984EA3", "Patient received nivolumab, vopratelimab prior to ICI study", 'group', NA_integer_))
  amadeus_tags <- rbind(amadeus_tags, c('pembrolizumab_spartalizumab_prior_ici_rx', 'pembrolizumab, spartalizumab', 'Pembrolizumab, Spartalizumab', "#E41A1C", "Patient received pembrolizumab, spartalizumab prior to ICI study", 'group', NA_integer_))

  #add tags for new prior treatments
  #There are prior treatments in this dataset that are not recorded in our database.
  # We will do this programmatically - you can adapt this code, delete it, etc
  prior_rx <- synapse_csv_id_to_tbl(syn, "syn64423811") %>%
    dplyr::filter(prior_info == "Prior_Rx") %>%
    dplyr::select(tag_name, all_drugs) %>%
    dplyr::distinct() %>%
    dplyr::filter(!tag_name %in% iatlasGraphQLClient::query_tags()$tag_name) %>%  #this checcks if the new tags are not already in the iatlas database
    dplyr::mutate(
      short_display= gsub(";", ", ", all_drugs),
      long_display= gsub(";", ", ", all_drugs),
      color= "",
      description = paste("Patient received", long_display, "prior to ICI study"),
      tag_type = "group",
      order = 53 + dplyr::row_number() #update number, or set to NA
    ) %>%
    dplyr::select(
      name = tag_name,
      short_display,
      long_display,
      color,
      description,
      tag_type,
      order
    )

  new_colors <- Polychrome::createPalette(nrow(prior_rx), "#FFA9A3") #creating new colors
  prior_rx$color <- new_colors

  amadeus_tags <- rbind(amadeus_tags, prior_rx)
  amadeus_tags <- rbind(amadeus_tags, parent_tags)

  amadeus_tags <- amadeus_tags %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    amadeus_tags,
    "", #UPDATE
    "tags"
  )


}
