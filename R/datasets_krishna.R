dataset_krishna <- function(){

  require(magrittr)
  require(rlang)

  syn <- create_synapse_login()

  datasets <-
    dplyr::tibble("display" = character(), "dataset_type" = character()) %>%
    dplyr::add_row("display" = "Krishna 2021 - ccRCC, PD-1", "dataset_type" = "analysis") %>%
    dplyr::mutate("name" = "Krishna_2021") %>%
    dplyr::arrange(.data$name) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "datasets"
    )
  synapse_store_table_as_csv(
    syn,
    datasets,
    "syn59194827",
    "datasets"
  )

}
