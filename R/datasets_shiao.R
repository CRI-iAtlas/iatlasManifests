dataset_shiao <- function(){

  require(magrittr)
  require(rlang)

  syn <- create_synapse_login()

  datasets <-
    dplyr::tibble("display" = character(), "dataset_type" = character()) %>%
    dplyr::add_row("display" = "Shiao 2024 - BRCA, PD-1", "dataset_type" = "scrna") %>%
    dplyr::mutate("name" = "Shiao_2024") %>%
    dplyr::arrange(.data$name) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "datasets"
    )
  synapse_store_table_as_csv(
    syn,
    datasets,
    "syn58398531",
    "datasets"
  )

}

