tags_htan_ohsu <- function() {

  require(magrittr)
  require(iatlasGraphQLClient)
  require(rlang)
  require(Polychrome)
  syn <- create_synapse_login()

  #we will use a table with consolidated prince annotation that was created with htan_create_wide_tag_table.R
  #load wide table with clinical annotation for HTAN OHSU
  htan_tags <- synapse_csv_id_to_tbl(syn, "syn52570142")
  htan_labels <- synapse_csv_id_to_tbl(syn, "syn52576077")

  #load the current ici tags
  db_tags <- iatlasGraphQLClient::query_tags_with_parent_tags()

  #Adding new parent groups

  #check if we added a new parent group
  new_parent_groups <- colnames(htan_tags)[!colnames(htan_tags) %in% db_tags$parent_tag_name]
  #the only new columns are HTAN.Biospecimen.ID and HTAN.Parent.ID


  #check if we have new levels
  htan_categories <- htan_tags %>%
    tidyr::pivot_longer(-c("HTAN.Biospecimen.ID", "HTAN.Parent.ID"), names_to = "parent_group", values_to = "tag") %>%
    dplyr::select(parent_group, tag) %>%
    dplyr::distinct()

  #new levels that need to be added
  new_categories <- htan_categories %>%
    dplyr::filter(!tag %in% db_tags$tag_name)

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
  #order_seed[["TCGA_Study"]]$order <- 34

  new_colors_per_group <-
    purrr::set_names(unique(new_categories$parent_group)) %>%
    purrr::map(.f= function(category){

      n_groups <- new_categories %>%
        dplyr::filter(parent_group == category) %>%
        nrow()

      group_colors <- db_tags %>%
        dplyr::filter(stringr::str_detect(tag_name, tolower(category))) %>%
        dplyr::filter(!is.na(tag_color)) %>%
        dplyr::pull(tag_color)

      if(length(group_colors)>0){
        new_colors <- Polychrome::createPalette(n_groups, group_colors)
        data.frame(new_colors[!is.na(names(new_colors))])
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
      "color" = NA_character_
    )


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
    dplyr::select(
      "name" = tag,
      "short_display",
      "long_display",
      "color",
      "description",
      "tag_type",
      "order"
    ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "tags"
    )

  # synapse_store_table_as_csv(
  #   syn,
  #   tags,
  #   "", #replace
  #   "tags"
  # )


}
