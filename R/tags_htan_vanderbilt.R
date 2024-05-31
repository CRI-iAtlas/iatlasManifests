tags_htan <- function() {

  require(magrittr)
  require(iatlasGraphQLClient)
  require(rlang)
  require(Polychrome)
  syn <- create_synapse_login()

  #we will use a table with consolidated htan annotation that was created with htan_create_wide_tag_table.R
  #load wide table with clinical annotation for HTAN MSK and Vanderbilt
  htan_tags <- synapse_csv_id_to_tbl(syn, "syn53623123") #vanderbilt

  htan_labels <- synapse_csv_id_to_tbl(syn, "syn53627469")#vanderbilt

  #load the current ici tags
  db_tags <- iatlasGraphQLClient::query_tags_with_parent_tags()

  synapse_tags <-
    synapse_csv_id_to_tbl(syn, "syn51613683") %>% #ici tags
    dplyr::add_row(
      synapse_csv_id_to_tbl(syn, "syn51080176")  #add tags from TCGA
    )
  #1. ADDING NEW PARENT GROUP

  #check if we added a new parent group
  parent_groups <- colnames(dplyr::select(htan_tags, -c("HTAN.Biospecimen.ID", "HTAN.Parent.ID")))
  new_parent_groups <- parent_groups[!parent_groups %in% db_tags$parent_tag_name]
  #we added "Tumor_tissue_type" and "Polyp_Histology"

  parent_tags<- dplyr::tibble("name"= character(),
                              "short_display"= character(),
                              "long_display"= character(),
                              "color"= character(),
                              "description"= character(),
                              "tag_type"= character(),
                              "order"= integer()) %>%
    dplyr::add_row("name"= "Tumor_tissue_type",
                   "short_display"= "Tumor tissue type",
                   "long_display"= "Tumor tissue type",
                   "color"= NA_character_,
                   "description"= "Type of tumor tissue collected",
                   "tag_type"= "parent_group",
                   "order"= 23) %>%
    dplyr::add_row("name"= "Polyp_Histology",
                   "short_display"= "Polyp Histology",
                   "long_display"= "Polyp Histology",
                   "color"= NA_character_,
                   "description"= "Polyp Histology",
                   "tag_type"= "parent_group",
                   "order"= 24)

  #2. ADDING NEW GROUPS

  #check if we have new levels
  htan_categories <- htan_tags %>%
    tidyr::pivot_longer(-c("HTAN.Biospecimen.ID", "HTAN.Parent.ID"), names_to = "parent_group", values_to = "tag") %>%
    dplyr::select(parent_group, tag) %>%
    dplyr::distinct()

  #new levels that need to be added
  new_categories <- htan_categories %>%
    dplyr::filter(!tag %in% synapse_tags$name)

  #save table for future use
  readr::write_csv(new_categories, "htan_vanderbilt_new_tags.csv", na = "")
  synapse_store_file(syn, "htan_vanderbilt_new_tags.csv", "syn52118831", "tags")

  description_templates <-
    purrr::set_names(unique(new_categories$parent_group)) %>%
    purrr::map(.f= function(category){
      na_labels <- c("na_|unknown_|none_")

      db_tags %>%
        dplyr::filter(tag_type == "group") %>%
        dplyr::filter(stringr::str_detect(tag_name, tolower(category))) %>%
        dplyr::filter(!stringr::str_detect(tag_name, na_labels)) %>%
        dplyr::select(tag_short_display, tag_characteristics) %>%
        dplyr::slice_head()

    })

#Manually adding template for new categories
  description_templates[["Tumor_tissue_type"]] <- data.frame(
    "tag_short_display" = "XYZ",
    "tag_characteristics" = "tumor tissue collected is XYZ"
  )
  description_templates[["Polyp_Histology"]] <- data.frame(
    "tag_short_display" = "XYZ",
    "tag_characteristics" = "Histology of polyp is XYZ"
  )
  description_templates[["TCGA_Study"]] <- data.frame(
    "tag_short_display" = "Not available",
    "tag_characteristics" = "Not available"
  )

  description_templates[["TCGA_Subtype"]] <- data.frame(
    "tag_short_display" = "Not available",
    "tag_characteristics" = "Not available"
  )

  order_seed <-
    purrr::set_names(unique(new_categories$parent_group)) %>%
    purrr::map(.f= function(category){
      na_labels <- c("na_|unknown_|none_")

      db_tags %>%
        dplyr::filter(tag_type == "group") %>%
        dplyr::filter(stringr::str_detect(tag_name, tolower(category))) %>%
        dplyr::filter(!stringr::str_detect(tag_name, na_labels)) %>%
        dplyr::select(tag_order) %>%
        dplyr::slice_tail() %>%
        as.vector()

    })
  #we need to add manually data for the new parent groups, and for the ones that have NA for other levels
  order_seed[["Tumor_tissue_type"]]$tag_order <- 1
  order_seed[["Polyp_Histology"]]$tag_order <- 1
  order_seed[["TCGA_Study"]]$tag_order <- 34
  order_seed[["TCGA_Subtype"]]$tag_order <- NA_integer_

  add_new_color <- function(n_groups, group_colors){
    new_colors <- Polychrome::createPalette(n_groups, group_colors)
    data.frame(new_colors[!is.na(names(new_colors))])
  }
  new_colors_per_group <-
    purrr::set_names(unique(new_categories$parent_group)) %>%
    purrr::map(.f= function(category){

      n_groups <- new_categories %>%
        dplyr::filter(parent_group == category) %>%
        nrow()

      group_colors <- synapse_tags %>%
        dplyr::filter(stringr::str_detect(name, tolower(category))) %>%
        dplyr::filter(!is.na(color)) %>%
        dplyr::pull(color)

      if(length(group_colors)>0){
        add_new_color(n_groups, group_colors)
      }else{
        return(NULL)
      }
    }) %>%
    purrr::list_rbind(names_to = "parent_group") %>%
    dplyr::arrange(parent_group)

  colnames(new_colors_per_group) <- c("parent_group", "color")
  rownames(new_colors_per_group) <- NULL

  new_colors_per_group <- new_colors_per_group %>%
    dplyr::add_row(
      "parent_group" = "TCGA_Subtype",
      "color" = "#868A88"
    ) %>%
    dplyr::add_row(
      "parent_group" = "TCGA_Study",
      "color" = "#868A88"
    ) %>%
    dplyr::add_row(
      "parent_group" = "Tumor_tissue_type",
      "color" = RColorBrewer::brewer.pal(nrow(dplyr::filter(new_categories, parent_group == "Tumor_tissue_type")), "Paired")
    ) %>%
    dplyr::add_row(
      "parent_group" = "Polyp_Histology",
      "color" = RColorBrewer::brewer.pal(nrow(dplyr::filter(new_categories, parent_group == "Polyp_Histology")), "Paired")
    ) %>%
    dplyr::arrange(parent_group)



  full_tags <- new_categories %>%
    dplyr::arrange(parent_group) %>%
    tibble::add_column("color" = new_colors_per_group$color) %>%
    dplyr::inner_join(htan_labels, by = dplyr::join_by("tag" == "name")) %>%
    dplyr::group_by(parent_group) %>%
    dplyr::arrange(short_display) %>%
    dplyr::mutate("position" = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      "long_display" = short_display,
      "description" = gsub(description_templates[[parent_group]]$tag_short_display,
                           short_display,
                           description_templates[[parent_group]]$tag_characteristics,
                           ignore.case = TRUE),
      "tag_type" = "group",
      "order" = sum(.data$position, order_seed[[parent_group]]$tag_order)
    ) %>%
    dplyr::mutate( #making sure the NAs entries have the right info
      "color" = dplyr::case_when(
        long_display == "Not available" ~ "#868A88",
        long_display == "Unknown" ~ "#6D7575",
        TRUE ~ color)
    ) %>%
    dplyr::select(
      "name" = tag,
      "short_display",
      "long_display",
      "color",
      "description",
      "tag_type",
      "order"
    ) %>%
    dplyr::add_row(parent_tags) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "tags"
    )

  synapse_store_table_as_csv(
    syn,
    full_tags,
    "syn53697421",
    "tags"
  )


}
