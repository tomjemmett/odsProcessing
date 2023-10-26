process_successors <- function(successors) {
  process_successor <- function(successor) {
    c(
      type = successor$Type,
      process_dates(successor),
      id = attr(successor$Target$OrgId, "extension")
    )
  }

  successors |>
    unname() |>
    purrr::map(process_successor) |>
    dplyr::bind_rows()
}
