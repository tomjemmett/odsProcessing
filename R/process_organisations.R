process_ods_organisations <- function(ods_xml) {
  process_org <- function(org) {
    res <- c(
      org_name = org$Name,
      process_dates(org),
      org_id = attr(org$OrgId, "extension"),
      status = attr(org$Status, "value"),
      last_change_date = lubridate::ymd(attr(org$LastChangeDate, "value"))
    ) |>
      dplyr::bind_rows()

    res$locations <- list(process_locations(org$GeoLoc))
    res$roles <- list(process_roles(org$Roles))
    res$rels <- list(process_rels(org$Rels))
    res$successors <- list(process_successors(org$Succs))

    res
  }

  future::plan(future::multicore)

  ods <- ods_xml |>
    xml2::xml_find_all("./Organisations/Organisation") |>
    furrr::future_map(
      purrr::compose(process_org, xml2::as_list),
      .progress = TRUE
    ) |>
    dplyr::bind_rows() |>
    tidyr::unnest("locations") |>
    janitor::clean_names() |>
    dplyr::relocate("addr_ln2", "addr_ln3", .after = "addr_ln1") |>
    dplyr::relocate("org_id", .before = "org_name") |>
    dplyr::mutate(
      dplyr::across("last_change_date", as.Date)
    )

  future::plan(future::sequential)

  ods
}

extract_ods_organisations <- function(ods_xml) {
  ods <- process_ods_organisations(ods_xml)

  ods |>
    dplyr::select("org_id", "roles") |>
    tidyr::unnest("roles") |>
    readr::write_csv("ods_roles.csv")

  ods |>
    dplyr::select("org_id", "rels") |>
    tidyr::unnest("rels") |>
    readr::write_csv("ods_rels.csv")

  ods |>
    dplyr::select(from = "org_id", "successors") |>
    tidyr::unnest("successors") |>
    dplyr::filter(.data[["type"]] == "Successor") |>
    dplyr::select(-"type") |>
    dplyr::rename(to = "id") |>
    dplyr::relocate("to", .after = "from") |>
    readr::write_csv("ods_successors.csv")

  ods |>
    dplyr::select(-"roles", -"rels", -"successors") |>
    readr::write_csv("ods_orgs.csv")

  invisible(NULL)
}
