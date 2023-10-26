process_ods_concepts <- function(ods_xml) {
  concepts <- ods_xml |>
    xml2::xml_find_all("./CodeSystems/CodeSystem/concept") |>
    purrr::map(xml2::xml_attrs) |>
    dplyr::bind_rows() |>
    dplyr::select(-"code") |>
    tibble::deframe()

  primary_roles <- ods_xml |>
    xml2::xml_find_all("./Manifest/PrimaryRoleScope/PrimaryRole") |>
    purrr::map(xml2::xml_attrs) |>
    dplyr::bind_rows() |>
    tibble::deframe()

  list(
    primary_roles = as.list(primary_roles),
    other_concepts = as.list(
      concepts[setdiff(names(concepts), names(primary_roles))]
    )
  )
}

extract_ods_concepts <- function(ods_xml) {
  jsonlite::write_json(
    process_ods_concepts(ods_xml),
    "ods_concepts.json",
    auto_unbox = TRUE,
    pretty = TRUE
  )
}
