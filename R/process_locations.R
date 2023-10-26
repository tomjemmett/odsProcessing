process_locations <- function(loc) {
  loc |>
    unname() |>
    purrr::map(purrr::flatten) |>
    dplyr::bind_rows()
}
