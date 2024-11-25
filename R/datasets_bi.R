dataset_bi <- function(){

  require(magrittr)
  require(rlang)


  syn <- create_synapse_login()

  datasets <-
    dplyr::tibble("display" = character(), "dataset_type" = character()) %>%
    dplyr::add_row("display" = "Bi 2021 - ccRCC - PD-1", "dataset_type" = "scrna") %>%
    dplyr::mutate("name" = "Bi_2021") %>%
    dplyr::arrange(.data$name) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n())
    )

  synapse_store_table_as_csv(
    syn,
    datasets,
    "syn60529985",
    "datasets"
  )

}
