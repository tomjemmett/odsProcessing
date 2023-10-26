run <- function(path = getwd()) {
  dir.create(path, FALSE, TRUE)
  withr::local_dir(path)

  ods_xml <- get_latest_ods_file() |>
    xml2::read_xml()

  extract_ods_concepts(ods_xml)
  extract_ods_organisations(ods_xml)
}
