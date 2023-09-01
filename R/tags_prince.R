tags_prince <- function() {

  require(magrittr)
  require(rlang)
  require(Polychrome)
  syn <- create_synapse_login()

  #we will use a table with consolidated prince annotation that was created with prince_tags_function.R
  #load wide table with clinical annotation for PRINCE
  prince_tags <- synapse_csv_id_to_tbl(syn, "syn52349216")
  prince_labels <- synapse_csv_id_to_tbl(syn, "syn52366476")

  #load the current ici tags
  ici_tags <- synapse_csv_id_to_tbl(syn, "syn51613683") # tags_ici

  #check if we added a new parent group
  new_parent_groups <- colnames(prince_tags)[!colnames(prince_tags) %in%ici_tags$name]
  #the only new columns are sample.id and subject.id, but we don't need to add them

  #check if we have new levels
  prince_categories <- prince_tags %>%
    tidyr::pivot_longer(-c("sample.id", "subject.id", "timepoint.relative.order"), names_to = "parent_group", values_to = "tag") %>%
    dplyr::select(parent_group, tag) %>%
    dplyr::distinct()

  #new levels that need to be added
  new_categories <- prince_categories %>%
    dplyr::filter(!tag %in% ici_tags$name)

  description_templates <-
    purrr::set_names(unique(new_categories$parent_group)) %>%
    purrr::map(.f= function(category){
    na_labels <- c("na_|unknown_|none_")

    ici_tags %>%
      dplyr::filter(stringr::str_detect(name, tolower(category))) %>%
      dplyr::filter(!stringr::str_detect(name, na_labels)) %>%
      dplyr::select(short_display, description) %>%
      dplyr::slice_head()

  })

  description_templates[["TCGA_Study"]] <- data.frame(
    "short_display" = "PDAC",
    "description" = "PDAC: pancreatic ductal adenocarcinoma"
    )

  order_seed <-
    purrr::set_names(unique(new_categories$parent_group)) %>%
    purrr::map(.f= function(category){
      na_labels <- c("na_|unknown_|none_")

      ici_tags %>%
        dplyr::filter(stringr::str_detect(name, tolower(category))) %>%
        dplyr::filter(!stringr::str_detect(name, na_labels)) %>%
        dplyr::select(order) %>%
        dplyr::slice_tail() %>%
        as.vector()

    })
  order_seed[["TCGA_Study"]]$order <- 34

  new_colors_per_group <-
    purrr::set_names(unique(dplyr::filter(new_categories, parent_group != "TCGA_Study")$parent_group)) %>%
    purrr::map(.f= function(category){

    n_groups <- new_categories %>%
      dplyr::filter(parent_group == category) %>%
      nrow()

    group_colors <- ici_tags %>%
      dplyr::filter(stringr::str_detect(name, tolower(category))) %>%
      dplyr::pull(color)

    new_colors <- Polychrome::createPalette(n_groups,  group_colors)
    data.frame(new_colors[!is.na(names(new_colors))])

  }) %>%
    purrr::list_rbind(names_to = "parent_group") %>%
    dplyr::arrange(parent_group)

  colnames(new_colors_per_group) <- c("parent_group", "color")
  rownames(new_colors_per_group) <- NULL
  new_colors_per_group <-  new_colors_per_group %>% dplyr::add_row(
    "parent_group" = "TCGA_Study",
    "color" = "#BDA626"
  )


  full_tags <- new_categories %>%
    dplyr::arrange(parent_group) %>%
    tibble::add_column("color" = new_colors_per_group$color) %>%
    dplyr::inner_join(prince_labels, by = dplyr::join_by("tag" == "name")) %>%
    dplyr::group_by(parent_group) %>%
    dplyr::arrange(short_display) %>%
    dplyr::mutate("position" = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      "long_display" = short_display,
      "description" = gsub(description_templates[[parent_group]]$short_display,
                           short_display,
                           description_templates[[parent_group]]$description,
                           ignore.case = TRUE),
      "tag_type" = "group",
      "order" = sum(.data$position, order_seed[[parent_group]]$order)
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

  synapse_store_table_as_csv(
    syn,
    tags,
    "", #replace
    "tags"
  )


}
