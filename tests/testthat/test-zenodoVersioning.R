dbData = readRDS(system.file("extdata",
  "CH_Sempach_2024_SEP24_25_DataExtract.rds",
  package = "birdscanR"
))
metadataFile = system.file("extdata", "dataPackage", "metadataTemplate.yaml",
  package = "birdscanR"
)
metadataList = yaml::read_yaml(metadataFile)

test_that("Zenodo identifiers and related identifiers are parsed and merged", {
  expect_identical(birdscanR:::.zenodoRecordId("https://doi.org/10.5281/zenodo.648124"), 648124L)
  expect_identical(birdscanR:::.zenodoRecordId("10.5072/zenodo.648124"), 648124L)
  expect_identical(birdscanR:::.zenodoRecordId("https://zenodo.org/records/123"), 123L)
  expect_identical(birdscanR:::.zenodoRecordId("123"), 123L)
  expect_identical(birdscanR:::.zenodoRecordId(123), 123L)
  expect_error(birdscanR:::.zenodoRecordId("https://doi.org/10.1111/ecog.04025"), "Not a Zenodo DOI")
  expect_false(birdscanR:::.isZenodoId(NULL))
  expect_false(birdscanR:::.isZenodoId("https://doi.org/10.1111/ecog.04025"))

  converted = birdscanR:::.zenodoRelatedIdentifiers(data.frame(
    relationType = "IsPartOf", relatedIdentifier = "https://doi.org/10.5281/zenodo.900"
  ))
  expect_identical(converted, list(list(identifier = "https://doi.org/10.5281/zenodo.900", relation = "isPartOf")))

  # Zenodo stores DOIs without URL prefix; these are the same identifier
  current = list(list(identifier = "10.5281/zenodo.900", relation = "isPartOf", scheme = "doi"))
  merged = birdscanR:::.mergeZenodoRelatedIdentifiers(current, c(converted, list(list(identifier = "10.5281/zenodo.901", relation = "isPartOf"))))
  expect_length(merged, 2)
  expect_identical(merged[[1]], current[[1]])
  expect_identical(merged[[2]]$identifier, "10.5281/zenodo.901")
})

test_that(".addDoiToPackage() replaces the DOI of the previous version in the citation", {
  package = list(name = "x", title = "t", bibliographicCitation = "A (2026) t.")
  package = birdscanR:::.addDoiToPackage(package, "10.5281/zenodo.101")
  expect_identical(names(package)[1:2], c("name", "id"))
  expect_identical(package$bibliographicCitation, "A (2026) t. https://doi.org/10.5281/zenodo.101")
  package = birdscanR:::.addDoiToPackage(package, "10.5281/zenodo.102")
  expect_identical(package$id, "https://doi.org/10.5281/zenodo.102")
  expect_identical(package$bibliographicCitation, "A (2026) t. https://doi.org/10.5281/zenodo.102")
})

test_that("uploadDataPackageToZenodo() creates a new version of a published record", {
  skip_if_not_installed("httr2")
  outDir = withr::local_tempdir()
  md = metadataList
  md$id = "https://doi.org/10.5281/zenodo.100"
  md$version = "1.0.1"
  createDataPackage(dbExtract = dbData, metadata = md, pulseTypeSelection = "S", outputDirPath = outDir)
  packageDir = file.path(outDir, "CH_Sempach_2024")

  log = newLog()
  newDraft = list(
    id = 102, files = list(list(id = "f1"), list(id = "f2")),
    metadata = list(
      title = "old title", doi = "10.5281/zenodo.101",
      prereserve_doi = list(doi = "10.5281/zenodo.102", recid = 102) # the sandbox reports the Zenodo prefix
    ),
    links = list(bucket = paste0(base, "/api/files/bucket-102"), html = paste0(base, "/deposit/102"))
  )
  httr2::local_mocked_responses(mockRouter(log, list(
    "GET /api/records/100/versions/latest$" = function(req) list(id = 101),
    "GET /api/deposit/depositions/101$" = function(req) {
      list(id = 101, submitted = TRUE, state = "done", metadata = list(
        version = "1.0.0",
        related_identifiers = list(list(identifier = "10.5281/zenodo.900", relation = "isPartOf", scheme = "doi"))
      ))
    },
    "POST /api/deposit/depositions/101/actions/newversion$" = function(req) {
      list(id = 101, links = list(latest_draft = paste0(base, "/api/deposit/depositions/102")))
    },
    "GET /api/deposit/depositions/102$" = function(req) newDraft,
    "DELETE /api/deposit/depositions/102/files/" = function(req) httr2::response(status_code = 204),
    "PUT /api/files/bucket-102/" = function(req) list(key = basename(req$url)),
    "PUT /api/deposit/depositions/102$" = function(req) newDraft
  )))

  expect_message(
    draft <- uploadDataPackageToZenodo(packageDir, token = "test-token", sandbox = TRUE, community = NULL),
    "Draft of a new version"
  )
  expect_true(draft$newVersion)
  expect_identical(draft$id, 102L)
  expect_identical(draft$doi, "10.5072/zenodo.102")

  calls = requestSummary(log)
  expect_identical(calls[1:6], c(
    "GET /api/records/100/versions/latest",
    "GET /api/deposit/depositions/101",
    "POST /api/deposit/depositions/101/actions/newversion",
    "GET /api/deposit/depositions/102",
    "DELETE /api/deposit/depositions/102/files/f1",
    "DELETE /api/deposit/depositions/102/files/f2"
  ))
  expect_identical(calls[7], "PUT /api/deposit/depositions/102")
  expect_identical(sum(grepl("^PUT /api/files/bucket-102/", calls)), 6L)
  expect_identical(calls[length(calls)], "PUT /api/deposit/depositions/102")
  expect_false(any(grepl("publish", calls)))

  # The DOI of the new version is reserved: no old DOI is sent
  reserveBody = log$requests[[7]]$body$metadata
  expect_null(reserveBody$doi)
  expect_true(reserveBody$prereserve_doi)

  # The metadata keep the IsPartOf link of the previous version
  related = log$requests[[length(log$requests)]]$body$metadata$related_identifiers
  relations = vapply(related, function(r) paste(r$relation, r$identifier), character(1))
  expect_true("isCompiledBy https://github.com/BirdScanCommunity/birdscanR" %in% relations)
  expect_true("isPartOf 10.5281/zenodo.900" %in% relations)

  # The package now carries the DOI of the new version, also in the citation
  package = jsonlite::read_json(file.path(packageDir, "datapackage.json"))
  expect_identical(package$id, "https://doi.org/10.5072/zenodo.102")
  expect_match(package$bibliographicCitation, "zenodo.102$")
  expect_false(grepl("zenodo.100", package$bibliographicCitation))
})

test_that("uploadDataPackageToZenodo() checks whether a new version is possible", {
  skip_if_not_installed("httr2")
  outDir = withr::local_tempdir()
  createDataPackage(dbExtract = dbData, metadata = metadataList, outputDirPath = outDir)
  packageDir = file.path(outDir, "CH_Sempach_2024")
  httr2::local_mocked_responses(function(req) stop("No request expected."))
  expect_error(
    uploadDataPackageToZenodo(packageDir, token = "test-token", newVersion = TRUE),
    "needs the DOI of the published Zenodo record"
  )

  # The previous version must be published
  md = metadataList
  md$id = "https://doi.org/10.5281/zenodo.100"
  outDir2 = withr::local_tempdir()
  createDataPackage(dbExtract = dbData, metadata = md, outputDirPath = outDir2)
  log = newLog()
  httr2::local_mocked_responses(mockRouter(log, list(
    "GET /versions/latest$" = function(req) httr2::response_json(status_code = 404, body = list(message = "Not found")),
    "GET /api/deposit/depositions/100$" = function(req) list(id = 100, submitted = FALSE, state = "unsubmitted")
  )))
  expect_error(
    uploadDataPackageToZenodo(file.path(outDir2, "CH_Sempach_2024"), token = "test-token", sandbox = TRUE),
    "is not published yet"
  )
})

publishedRecord = function(state = "done", submitted = TRUE) {
  list(
    id = 101, submitted = submitted, state = state,
    links = list(html = paste0(base, "/deposit/101")),
    metadata = list(
      title = "A campaign", doi = "10.5281/zenodo.101",
      prereserve_doi = list(doi = "10.5281/zenodo.101", recid = 101),
      related_identifiers = list(list(identifier = "https://github.com/BirdScanCommunity/birdscanR", relation = "isCompiledBy", scheme = "url"))
    )
  )
}
isPartOf = list(list(relationType = "IsPartOf", relatedIdentifier = "https://doi.org/10.5281/zenodo.900"))

test_that("addZenodoRelatedIdentifiers() edits a published record, and publishes only on request", {
  skip_if_not_installed("httr2")
  routes = list(
    "GET /api/records/100/versions/latest$" = function(req) list(id = 101),
    "GET /api/deposit/depositions/101$" = function(req) publishedRecord(),
    "POST /actions/(edit|publish)$" = function(req) list(id = 101),
    "PUT /api/deposit/depositions/101$" = function(req) list(id = 101)
  )

  log = newLog()
  httr2::local_mocked_responses(mockRouter(log, routes))
  expect_message(
    result <- addZenodoRelatedIdentifiers("10.5281/zenodo.100", isPartOf, token = "test-token", sandbox = TRUE),
    "not published yet"
  )
  expect_identical(result$added, 1L)
  expect_false(result$published)
  expect_identical(requestSummary(log), c(
    "GET /api/records/100/versions/latest",
    "GET /api/deposit/depositions/101",
    "POST /api/deposit/depositions/101/actions/edit",
    "PUT /api/deposit/depositions/101"
  ))
  body = log$requests[[4]]$body$metadata
  expect_null(body$prereserve_doi)
  expect_null(body$doi)
  expect_identical(body$title, "A campaign")
  expect_length(body$related_identifiers, 2)
  expect_identical(body$related_identifiers[[2]], list(identifier = "https://doi.org/10.5281/zenodo.900", relation = "isPartOf"))

  log = newLog()
  httr2::local_mocked_responses(mockRouter(log, routes))
  result = suppressMessages(addZenodoRelatedIdentifiers("10.5281/zenodo.100", isPartOf, token = "test-token", sandbox = TRUE, publish = TRUE))
  expect_true(result$published)
  expect_identical(requestSummary(log)[5], "POST /api/deposit/depositions/101/actions/publish")
})

test_that("addZenodoRelatedIdentifiers() changes nothing if the links exist, and never publishes a draft", {
  skip_if_not_installed("httr2")
  log = newLog()
  existing = publishedRecord()
  existing$metadata$related_identifiers[[2]] = list(identifier = "10.5281/zenodo.900", relation = "isPartOf", scheme = "doi")
  httr2::local_mocked_responses(mockRouter(log, list(
    "GET /versions/latest$" = function(req) list(id = 101),
    "GET /api/deposit/depositions/101$" = function(req) existing
  )))
  expect_message(
    result <- addZenodoRelatedIdentifiers(101, isPartOf, token = "test-token", sandbox = TRUE, publish = TRUE),
    "nothing changed"
  )
  expect_identical(result$added, 0L)
  expect_length(log$requests, 2)

  log = newLog()
  httr2::local_mocked_responses(mockRouter(log, list(
    "GET /versions/latest$" = function(req) httr2::response_json(status_code = 404, body = list(message = "Not found")),
    "GET /api/deposit/depositions/101$" = function(req) publishedRecord(state = "unsubmitted", submitted = FALSE),
    "PUT /api/deposit/depositions/101$" = function(req) list(id = 101)
  )))
  result = suppressMessages(addZenodoRelatedIdentifiers(101, isPartOf, token = "test-token", sandbox = TRUE, publish = TRUE))
  expect_false(result$published)
  calls = requestSummary(log)
  expect_false(any(grepl("actions/", calls)))
  expect_true(log$requests[[3]]$body$metadata$prereserve_doi)
})

test_that("addZenodoRelatedIdentifiers() discards the edit when the update fails", {
  skip_if_not_installed("httr2")
  log = newLog()
  httr2::local_mocked_responses(mockRouter(log, list(
    "GET /versions/latest$" = function(req) list(id = 101),
    "GET /api/deposit/depositions/101$" = function(req) publishedRecord(),
    "POST /actions/(edit|discard)$" = function(req) list(id = 101),
    "PUT /api/deposit/depositions/101$" = function(req) {
      httr2::response_json(status_code = 400, body = list(
        message = "Validation error.",
        errors = list(list(field = "metadata.related_identifiers", message = "Invalid relation."))
      ))
    }
  )))
  err = expect_error(addZenodoRelatedIdentifiers(101, isPartOf, token = "test-token", sandbox = TRUE))
  expect_match(conditionMessage(err), "Invalid relation")
  expect_match(conditionMessage(err), "were not changed")
  expect_identical(requestSummary(log)[5], "POST /api/deposit/depositions/101/actions/discard")
})
