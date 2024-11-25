dataset_li <- function(){

  require(magrittr)
  require(rlang)


  syn <- create_synapse_login()

  datasets <-
    dplyr::tibble("display" = character(), "dataset_type" = character()) %>%
    dplyr::add_row("display" = "Li 2022 - ccRCC", "dataset_type" = "scrna") %>%
    dplyr::mutate("name" = "Li_2022") %>%
    dplyr::arrange(.data$name) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "datasets"
    )

  synapse_store_table_as_csv(
    syn,
    datasets,
    "syn60085087",
    "datasets"
  )

}

