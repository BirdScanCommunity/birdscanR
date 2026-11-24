dbData = readRDS(system.file("extdata",
  "CH_Sempach_2024_SEP24_25_DataExtract.rds",
  package = "birdscanR"
))
metadataFile = system.file("extdata", "dataPackage", "metadataTemplate.yaml",
  package = "birdscanR"
)
metadataList = yaml::read_yaml(metadataFile)
metadataList$contributors[[2]] = list(
  title = "Baptiste Schmid", givenName = "Baptiste", familyName = "Schmid",
  email = "baptiste.schmid@example.org",
  roles = c("ProjectLeader", "ContactPerson"), embargoRequestType = 1
)

# Write a data package to a temporary directory and return its directory
writeTestPackage = function(env = parent.frame()) {
  outDir = withr::local_tempdir(.local_envir = env)
  createDataPackage(dbExtract = dbData, metadata = metadataList, pulseTypeSelection = "S", outputDirPath = outDir)
  file.path(outDir, "CH_Sempach_2024")
}

# Mock of the Zenodo deposit API; records the requests in `log`
mockZenodo = function(log, failMetadata = FALSE) {
  function(req) {
    log$requests[[length(log$requests) + 1]] = list(
      method = req$method, url = req$url, body = req$body
    )
    if (req$method == "POST") {
      return(httr2::response_json(status_code = 201, body = list(
        id = 123,
        metadata = list(prereserve_doi = list(doi = "10.5072/zenodo.123")),
        links = list(
          bucket = "https://sandbox.zenodo.org/api/files/bucket-1",
          html = "https://sandbox.zenodo.org/deposit/123"
        )
      )))
    }
    if (grepl("/api/files/", req$url)) {
      return(httr2::response_json(status_code = 201, body = list(key = basename(req$url))))
    }
    if (failMetadata) {
      return(httr2::response_json(status_code = 400, body = list(
        message = "Validation error.",
        errors = list(list(field = "metadata.license", message = "Invalid licence."))
      )))
    }
    httr2::response_json(status_code = 200, body = list(id = 123))
  }
}

test_that(".zenodoMetadata() maps the package metadata to Zenodo", {
  packageDir = writeTestPackage()
  package = jsonlite::read_json(file.path(packageDir, "datapackage.json"), simplifyVector = FALSE)

  md = birdscanR:::.zenodoMetadata(package, community = "birdscanmr1community", today = as.Date("2025-01-01"))
  expect_identical(md$upload_type, "dataset")
  expect_identical(md$title, package$title)
  expect_identical(md$version, "1.0.0")
  expect_identical(md$license, "cc-by-4.0")
  expect_identical(md$access_right, "embargoed")
  expect_identical(md$embargo_date, "2029-09-25")
  expect_true(md$prereserve_doi)

  # Creators: all contributors; contributors: one entry per role, plus owner
  expect_identical(md$creators[[1]], list(
    name = "Haest, Birgen", affiliation = "Swiss Ornithological Institute",
    orcid = "0000-0002-8739-6460"
  ))
  expect_identical(md$creators[[2]]$name, "Schmid, Baptiste")
  contributorTypes = vapply(md$contributors, function(x) x$type, character(1))
  expect_identical(contributorTypes, c("ContactPerson", "DataCurator", "ProjectLeader", "ContactPerson", "RightsHolder"))
  expect_identical(md$contributors[[5]]$name, "Swiss Ornithological Institute")

  expect_identical(unclass(md$keywords), unlist(package$keywords))
  expect_identical(md$related_identifiers[[1]]$relation, "isCompiledBy")
  expect_identical(md$communities, list(list(identifier = "birdscanmr1community")))
  expect_equal(md$locations[[1]]$lat, package$site$latitude)
  expect_identical(md$dates[[1]], list(start = "2024-09-24", end = "2024-09-25", type = "Collected"))
  expect_match(md$description, "Suggested citation:")
  expect_match(md$notes, "embargo until 2029-09-25")
  expect_match(md$notes, "Baptiste Schmid \\(baptiste.schmid@example.org\\)")

  # After the embargo: open access, no embargo date or notes; no community
  mdOpen = birdscanR:::.zenodoMetadata(package, today = as.Date("2030-01-01"))
  expect_identical(mdOpen$access_right, "open")
  expect_null(mdOpen$embargo_date)
  expect_null(mdOpen$notes)
  expect_null(mdOpen$communities)
})

test_that("uploadDataPackageToZenodo() creates a draft, adds the DOI and uploads all files", {
  skip_if_not_installed("httr2")
  packageDir = writeTestPackage()
  log = new.env()
  log$requests = list()
  httr2::local_mocked_responses(mockZenodo(log))

  expect_message(
    draft <- uploadDataPackageToZenodo(packageDir, token = "test-token", sandbox = TRUE, community = NULL),
    "Reserved DOI: 10.5072/zenodo.123"
  )
  expect_identical(draft$id, 123L)
  expect_identical(draft$doi, "10.5072/zenodo.123")
  expect_identical(draft$url, "https://sandbox.zenodo.org/deposit/123")
  expect_setequal(draft$files, c(
    "datapackage.json", "site_data.csv.gz", "protocol_data.csv.gz", "echo_data.csv.gz",
    "blind_times.csv.gz", "sunrise_sunset.csv.gz"
  ))

  # Requests: create draft, upload 6 files, add metadata; never publish
  methods = vapply(log$requests, function(r) r$method, character(1))
  urls = vapply(log$requests, function(r) r$url, character(1))
  expect_identical(methods, c("POST", rep("PUT", 7)))
  expect_identical(urls[1], "https://sandbox.zenodo.org/api/deposit/depositions")
  expect_identical(sum(grepl("/api/files/bucket-1/", urls)), 6L)
  expect_identical(urls[8], "https://sandbox.zenodo.org/api/deposit/depositions/123")
  expect_false(any(grepl("publish", urls)))
  expect_null(log$requests[[8]]$body$data$metadata$communities)
  expect_identical(log$requests[[8]]$body$data$metadata$title, metadataList$title)

  # The DOI is written into the local package, which stays valid
  package = jsonlite::read_json(file.path(packageDir, "datapackage.json"))
  expect_identical(package$id, "https://doi.org/10.5072/zenodo.123")
  expect_match(package$bibliographicCitation, "https://doi.org/10.5072/zenodo.123$")
  expect_identical(names(package)[1:2], c("name", "id"))
  expect_true(validateDataPackage(packageDir)$valid)
})

test_that("uploadDataPackageToZenodo() reports Zenodo errors with the draft record", {
  skip_if_not_installed("httr2")
  packageDir = writeTestPackage()
  log = new.env()
  log$requests = list()
  httr2::local_mocked_responses(mockZenodo(log, failMetadata = TRUE))
  err = expect_error(uploadDataPackageToZenodo(packageDir, token = "test-token", sandbox = TRUE, community = NULL))
  expect_match(conditionMessage(err), "Invalid licence")
  expect_match(conditionMessage(err), "draft record 123 was created")
})

test_that("uploadDataPackageToZenodo() does not upload without token or with an invalid package", {
  skip_if_not_installed("httr2")
  packageDir = writeTestPackage()
  httr2::local_mocked_responses(function(req) stop("No request expected."))
  withr::local_envvar(ZENODO_TOKEN = "", ZENODO_SANDBOX_TOKEN = "")

  expect_error(uploadDataPackageToZenodo(packageDir), "No Zenodo token found")
  expect_error(uploadDataPackageToZenodo(tempdir(), token = "test-token"), "No 'datapackage.json' found")

  packageJson = file.path(packageDir, "datapackage.json")
  package = jsonlite::read_json(packageJson, simplifyVector = FALSE)
  package$title = NULL
  birdscanR:::.writePackageJson(package, packageJson)
  expect_output(
    expect_error(uploadDataPackageToZenodo(packageDir, token = "test-token"), "not valid and was not uploaded"),
    "NOT valid"
  )
})
