mutation_types_tcga <- function(){

  require(magrittr)
  require(rlang)
  syn <- create_synapse_login()

  mutation_types <-
    dplyr::tibble(
      "name" = "driver mutation",
      "display" = "Driver Mutation"
    ) %>%
    dplyr::mutate(
      "id" = uuid::UUIDgenerate(n = dplyr::n()),
      "Component" = "mutation_types"
    )

  synapse_store_table_as_csv(
    syn,
    mutation_types,
    "syn51671617",
    "mutation_types"
  )
}



