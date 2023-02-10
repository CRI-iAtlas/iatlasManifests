build_dataset_table <- function(){

  require(magrittr)
  require(rlang)

  datasets <-
    dplyr::tibble("display" = character(), type = character()) %>%
    dplyr::add_row("display" = c("TCGA", "PCAWG"), "type" = "analysis") %>%
    dplyr::add_row("display" = c("GTEX"), "type" = "other") %>%
    dplyr::mutate("name" = stringr::str_replace_all(.data$display, " ", "_")) %>%
    dplyr::arrange(.data$name) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "datasets"
    )

  readr::write_csv(datasets, "synapse_storage_manifest.csv")

}

