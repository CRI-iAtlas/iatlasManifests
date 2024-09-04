dataset_htan <- function(){

  require(magrittr)
  require(rlang)

  datasets <-
    dplyr::tibble("display" = character(), "dataset_type" = character()) %>%
    dplyr::add_row("display" = c("HTAN OHSU"), "dataset_type" = "ici") %>%
    dplyr::mutate("name" = stringr::str_replace_all(.data$display, " ", "_")) %>%
    dplyr::arrange(.data$name) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "datasets"
    )

}

