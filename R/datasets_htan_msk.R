dataset_htan <- function(){

  require(magrittr)
  require(rlang)

  datasets <-
    dplyr::tibble("display" = character(), "dataset_type" = character()) %>%
    dplyr::add_row("display" = "MSK - SCLC", "dataset_type" = "scrna") %>%
    dplyr::mutate("name" = "MSK") %>%
    dplyr::arrange(.data$name) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "datasets"
    )
  synapse_store_table_as_csv(
    syn,
    datasets,
    "syn53678185", #replace
    "datasets"
  )

}

