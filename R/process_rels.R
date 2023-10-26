process_rels <- function(rels) {
  process_rel <- function(rel) {
    c(
      rel_id = attr(rel, "id"),
      process_dates(rel),
      status = attr(rel$Status, "value"),
      target_id = attr(rel$Target$OrgId, "extension")
    )
  }

  rels |>
    unname() |>
    purrr::map(process_rel) |>
    dplyr::bind_rows()
}
