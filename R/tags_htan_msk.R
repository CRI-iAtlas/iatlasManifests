tags_htan_msk <- function() {

  require(magrittr)
  require(iatlasGraphQLClient)
  require(rlang)
  require(Polychrome)
  syn <- create_synapse_login()

  #we will use a table with consolidated htan annotation that was created with htan_create_wide_tag_table.R
  #load wide table with clinical annotation for HTAN MSK and Vanderbilt
  htan_tags <- synapse_csv_id_to_tbl(syn, "syn53605383")

  htan_labels <- synapse_csv_id_to_tbl(syn, "syn53605384")%>%
    dplyr::add_row(
      "parent_tag" = "TCGA_Study",
      "short_display" = "SCLC",
      "name" = "SCLC"
    )

  #load the current ici tags
  db_tags <- iatlasGraphQLClient::query_tags_with_parent_tags()
  #we also need to add the tags added in the HTAN Vanderbilt dataset
  vanderbilt_tags <- synapse_csv_id_to_tbl(syn, "syn53697423")

  #1. ADDING NEW PARENT GROUP

  #check if we added a new parent group
  parent_groups <- colnames(dplyr::select(htan_tags, -c("HTAN.Biospecimen.ID", "HTAN.Parent.ID")))
  new_parent_groups <- parent_groups[!parent_groups %in% db_tags$parent_tag_name]
  #we added "Tumor_tissue_type" and "Polyp_Histology", but we already added info about them in Vanderbilt, so no need to do it again

  #2. ADDINNG NEW GROUPS
  #check if we have new levels
  htan_categories <- htan_tags %>%
    tidyr::pivot_longer(-c("HTAN.Biospecimen.ID", "HTAN.Parent.ID"), names_to = "parent_group", values_to = "tag") %>%
    dplyr::select(parent_group, tag) %>%
    dplyr::distinct()

  #new levels that need to be added
  new_categories <- htan_categories %>%
    dplyr::filter(!tag %in% db_tags$tag_name) %>%
    dplyr::filter(!tag %in% vanderbilt_tags$name)

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
  description_templates[["TCGA_Study"]] <- data.frame(
    "tag_short_display" = "SCLC",
    "tag_characteristics" = "SCLC: Small Cell Lung Cancer"
  )

  description_templates[["TCGA_Subtype"]] <- data.frame(
    "tag_short_display" = "SCLC-A",
    "tag_characteristics" = "Subtype SCLC-A"
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
  order_seed[["Tumor_tissue_type"]]$tag_order <- 7
  order_seed[["TCGA_Study"]]$tag_order <- 33
  order_seed[["TCGA_Subtype"]]$tag_order <- NA_integer_


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
      "color" = RColorBrewer::brewer.pal(nrow(dplyr::filter(new_categories, parent_group == "TCGA_Subtype")), "Paired")
    ) %>%
    dplyr::add_row(
      "parent_group" = "TCGA_Study",
      "color" = "#BEA222"
    ) %>%
    dplyr::add_row(
      "parent_group" = "Tumor_tissue_type",
      "color" = RColorBrewer::brewer.pal(nrow(dplyr::filter(new_categories, parent_group == "Tumor_tissue_type")), "Paired")
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
    full_tags,
    "syn53697422",
    "tags"
  )


}
