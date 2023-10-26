process_dates <- function(x) {
  process_date <- function(d) {
    d <- purrr::map(d, attr, "value")

    type <- stringr::str_to_lower(d$Type)
    d$Type <- NULL

    d |>
      purrr::map(lubridate::ymd) |>
      purrr::set_names(
        \(.x) paste(type, stringr::str_to_lower(.x), sep = "_")
      )
  }

  x[names(x) == "Date"] |>
    purrr::map(process_date) |>
    purrr::flatten()
}
