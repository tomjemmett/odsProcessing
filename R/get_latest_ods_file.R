get_latest_ods_file <- function(apikey = Sys.getenv("TRUD_API_KEY")) {
  stopifnot("TRUD_API_KEY not set" = apikey != "")

  ods_xml <- httr::GET(
    "https://isd.digital.nhs.uk",
    path = c(
      "trud",
      "api",
      "v1",
      "keys",
      apikey,
      "items",
      "341",
      "releases"
    ),
    query = "latest"
  )

  tf_zip <- withr::local_tempfile()

  download.file(httr::content(ods_xml)$releases[[1]]$archiveFileUrl, tf_zip)

  tf_fullfile <- withr::local_file("fullfile.zip")

  unzip(tf_zip, files = tf_fullfile)

  xml_path <- unzip(tf_fullfile, list = TRUE)$Name[[1]]
  unzip(tf_fullfile, files = xml_path)

  xml_path
}
