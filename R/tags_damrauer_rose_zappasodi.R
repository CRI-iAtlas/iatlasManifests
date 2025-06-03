tags_damrauer_rose_zappasodi <- function() {

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
  # no new parent groups for these datasets

  #2. ADDING NEW GROUPS
  # New tags were saved as files. There are no new tags for the Zappasodi dataset
  new_tags <- synapse_csv_id_to_tbl(syn, "syn65986771") %>%
    dplyr::bind_rows(synapse_csv_id_to_tbl(syn, "syn65986579")) %>%
    dplyr::select(
      "name" = "tag_name",
      "short_display"= "tag_display",
      "description"
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      long_display = short_display,
      tag_type = "group",
      color = Polychrome::createPalette(nrow(.), "#FFA9A3"),
      order = 0,
      "id" = uuid::UUIDgenerate(n = dplyr::n())
        )

  synapse_store_table_as_csv(
    syn,
    new_tags,
    "syn66227465",
    "tags"
  )


}
