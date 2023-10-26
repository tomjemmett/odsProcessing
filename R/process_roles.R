process_roles <- function(roles) {
  process_role <- function(role) {
    c(
      role_id = attr(role, "id"),
      process_dates(role),
      status = attr(role$Status, "value")
    )
  }

  roles |>
    unname() |>
    purrr::map(process_role) |>
    dplyr::bind_rows()
}
