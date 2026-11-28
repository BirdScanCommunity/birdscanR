collectionTemplate = system.file("extdata", "dataPackage", "collectionMetadataTemplate.yaml",
  package = "birdscanR"
)
collectionMd = yaml::read_yaml(collectionTemplate)
collectionMd$members = list("https://doi.org/10.5072/zenodo.201", "10.5072/zenodo.202")

# Published member record (records API), latest version `id` of concept `concept`
memberRecord = function(id, concept, title, start, end) {
  list(
    id = id, doi = paste0("10.5072/zenodo.", id), conceptdoi = paste0("10.5072/zenodo.", concept),
    conceptrecid = as.character(concept),
    links = list(self_html = paste0(base, "/records/", id)),
    metadata = list(
      title = title, version = "1.0.0", publication_date = "2026-11-20",
      dates = list(list(start = start, end = end, type = "collected"))
    )
  )
}
# Published member deposition (deposit API)
memberDeposition = function(id, state = "done", related = list()) {
  list(
    id = id, submitted = TRUE, state = state,
    links = list(html = paste0(base, "/records/", id)),
    metadata = list(title = paste("Campaign", id), doi = paste0("10.5072/zenodo.", id), related_identifiers = related)
  )
}

test_that("createCollectionMetadataTemplate() writes a valid template", {
  filePath = file.path(withr::local_tempdir(), "collection.yaml")
  expect_identical(createCollectionMetadataTemplate(filePath), filePath)
  expect_true(file.exists(filePath))
  expect_error(createCollectionMetadataTemplate(filePath), "already exists")
  expect_length(birdscanR:::.checkCollectionMetadata(yaml::read_yaml(filePath)), 0)
})

test_that(".checkCollectionMetadata() reports problems", {
  md = collectionMd
  md$level = "campaign"
  md$members = list("https://doi.org/10.1111/ecog.04025")
  md$id = "not a doi"
  md$contributors[[1]]$roles = list("DataCurator")
  problems = birdscanR:::.checkCollectionMetadata(md)
  expect_true(any(grepl("'level' must be", problems)))
  expect_true(any(grepl("Not valid: https://doi.org/10.1111/ecog.04025", problems)))
  expect_true(any(grepl("'id' must be the DOI", problems)))
  expect_true(any(grepl("ContactPerson", problems)))
  # Contributors of collections need no embargoRequestType
  expect_false(any(grepl("embargoRequestType", problems)))

  md = collectionMd
  md$members = NULL
  expect_true(any(grepl("'members' is required", birdscanR:::.checkCollectionMetadata(md))))

  httr2::local_mocked_responses(function(req) stop("No request expected."))
  expect_error(uploadCollectionToZenodo(md, token = "test-token"), "not valid")
})

test_that("uploadCollectionToZenodo() creates a collection draft and links the members", {
  skip_if_not_installed("httr2")
  log = newLog()
  draft = list(
    id = 301, conceptrecid = "300",
    metadata = list(prereserve_doi = list(doi = "10.5072/zenodo.301", recid = 301)),
    links = list(bucket = paste0(base, "/api/files/bucket-301"), html = paste0(base, "/deposit/301"))
  )
  uploaded = NULL
  httr2::local_mocked_responses(mockRouter(log, list(
    "GET /api/records/201/versions/latest$" = function(req) memberRecord(211, 201, "Campaign A", "2024-03-01", "2024-11-30"),
    "GET /api/records/202/versions/latest$" = function(req) memberRecord(222, 202, "Campaign B", "2023-02-01", "2023-10-31"),
    "POST /api/deposit/depositions$" = function(req) draft,
    "PUT /api/files/bucket-301/members.csv$" = function(req) {
      uploaded <<- utils::read.csv(req$body$data, stringsAsFactors = FALSE)
      list(key = "members.csv")
    },
    "PUT /api/deposit/depositions/301$" = function(req) draft,
    # Member links: 211 can be changed, 222 belongs to another account
    "GET /api/records/211/versions/latest$" = function(req) list(id = 211),
    "GET /api/records/222/versions/latest$" = function(req) list(id = 222),
    "GET /api/deposit/depositions/211$" = function(req) memberDeposition(211),
    "GET /api/deposit/depositions/222$" = function(req) httr2::response_json(status_code = 403, body = list(message = "Permission denied.")),
    "POST /api/deposit/depositions/211/actions/edit$" = function(req) list(id = 211),
    "PUT /api/deposit/depositions/211$" = function(req) list(id = 211)
  )))

  expect_warning(
    expect_message(
      result <- uploadCollectionToZenodo(collectionMd, token = "test-token", sandbox = TRUE, community = NULL),
      "Draft collection record created"
    ),
    "could not be added to 1 member"
  )
  expect_identical(result$id, 301L)
  expect_identical(result$doi, "10.5072/zenodo.301")
  expect_identical(result$conceptDoi, "10.5072/zenodo.300")
  expect_false(result$newVersion)
  expect_identical(result$members$conceptDoi, c("https://doi.org/10.5072/zenodo.201", "https://doi.org/10.5072/zenodo.202"))
  expect_identical(result$members$doi, c("https://doi.org/10.5072/zenodo.211", "https://doi.org/10.5072/zenodo.222"))
  expect_identical(uploaded$conceptDoi, result$members$conceptDoi)
  expect_identical(uploaded$title, c("Campaign A", "Campaign B"))
  expect_identical(result$members$start, c("2024-03-01", "2023-02-01"))
  expect_identical(result$members$url, paste0(base, "/records/", c(211, 222)))
  expect_identical(result$links$added, c(1L, 0L))
  expect_match(result$links$error[2], "Permission denied")

  calls = requestSummary(log)
  expect_false(any(grepl("publish", calls)))

  # Collection metadata: HasPart links to the concept DOIs, other links, period
  metadataRequest = Filter(function(r) r$method == "PUT" && grepl("depositions/301$", r$url), log$requests)
  md = metadataRequest[[length(metadataRequest)]]$body$metadata
  relations = vapply(md$related_identifiers, function(r) paste(r$relation, r$identifier), character(1))
  expect_identical(relations, c(
    "hasPart https://doi.org/10.5072/zenodo.201",
    "hasPart https://doi.org/10.5072/zenodo.202",
    "isCompiledBy https://github.com/BirdScanCommunity/birdscanR"
  ))
  expect_identical(md$dates[[1]], list(start = "2023-02-01", end = "2024-11-30", type = "Collected"))
  expect_match(md$description, "contains 2 MR1 data packages")
  expect_match(md$description, "Campaign B")
  expect_match(md$description, "zenodo.301</p>$")
  expect_null(md$communities)
  expect_identical(md$contributors[[length(md$contributors)]], list(name = "Swiss Ornithological Institute", type = "RightsHolder"))

  # The member is linked to the concept DOI of the collection
  linkRequest = Filter(function(r) r$method == "PUT" && grepl("depositions/211$", r$url), log$requests)[[1]]
  linkRelations = vapply(linkRequest$body$metadata$related_identifiers, function(r) paste(r$relation, r$identifier), character(1))
  expect_identical(linkRelations, "isPartOf https://doi.org/10.5072/zenodo.300")
})

test_that("uploadCollectionToZenodo() creates a new version with the new member list", {
  skip_if_not_installed("httr2")
  log = newLog()
  md = collectionMd
  md$id = "https://doi.org/10.5072/zenodo.301"
  md$version = "1.1.0"
  newDraft = list(
    id = 302, conceptrecid = "300", files = list(list(id = "f1")),
    metadata = list(title = "old", doi = "10.5072/zenodo.301", prereserve_doi = list(doi = "10.5072/zenodo.302", recid = 302)),
    links = list(bucket = paste0(base, "/api/files/bucket-302"), html = paste0(base, "/deposit/302"))
  )
  httr2::local_mocked_responses(mockRouter(log, list(
    "GET /api/records/201/versions/latest$" = function(req) memberRecord(211, 201, "Campaign A", "2024-03-01", "2024-11-30"),
    "GET /api/records/202/versions/latest$" = function(req) memberRecord(222, 202, "Campaign B", "2023-02-01", "2023-10-31"),
    "GET /api/records/301/versions/latest$" = function(req) list(id = 301),
    "GET /api/deposit/depositions/301$" = function(req) {
      list(
        id = 301, submitted = TRUE, state = "done", conceptdoi = "10.5072/zenodo.300",
        metadata = list(version = "1.0.0", related_identifiers = list(
          list(identifier = "10.5072/zenodo.199", relation = "hasPart", scheme = "doi"),
          list(identifier = "10.1111/xyz", relation = "isDocumentedBy", scheme = "doi")
        ))
      )
    },
    "POST /api/deposit/depositions/301/actions/newversion$" = function(req) {
      list(id = 301, links = list(latest_draft = paste0(base, "/api/deposit/depositions/302")))
    },
    "GET /api/deposit/depositions/302$" = function(req) newDraft,
    "DELETE /api/deposit/depositions/302/files/f1$" = function(req) httr2::response(status_code = 204),
    "PUT /api/files/bucket-302/members.csv$" = function(req) list(key = "members.csv"),
    "PUT /api/deposit/depositions/302$" = function(req) newDraft
  )))

  expect_message(
    result <- uploadCollectionToZenodo(md, token = "test-token", sandbox = TRUE, community = NULL, linkMembers = FALSE),
    "new version of the collection"
  )
  expect_true(result$newVersion)
  expect_identical(result$doi, "10.5072/zenodo.302")
  expect_identical(result$conceptDoi, "10.5072/zenodo.300")
  expect_null(result$links)

  calls = requestSummary(log)
  expect_true("DELETE /api/deposit/depositions/302/files/f1" %in% calls)
  expect_false(any(grepl("publish|/actions/edit", calls)))

  # The removed member is dropped, the other link of the previous version kept
  last = log$requests[[length(log$requests)]]$body$metadata
  relations = vapply(last$related_identifiers, function(r) paste(r$relation, r$identifier), character(1))
  expect_false(any(grepl("zenodo.199", relations)))
  expect_true("isDocumentedBy 10.1111/xyz" %in% relations)
  expect_identical(sum(grepl("^hasPart", relations)), 2L)
  expect_identical(last$version, "1.1.0")
})

test_that("uploadCollectionToZenodo() refuses members listed twice", {
  skip_if_not_installed("httr2")
  md = collectionMd
  md$members = list("10.5072/zenodo.201", "10.5072/zenodo.211")
  log = newLog()
  httr2::local_mocked_responses(mockRouter(log, list(
    "GET /api/records/(201|211)/versions/latest$" = function(req) memberRecord(211, 201, "Campaign A", "2024-03-01", "2024-11-30")
  )))
  expect_error(
    uploadCollectionToZenodo(md, token = "test-token", sandbox = TRUE),
    "more than once"
  )
  expect_false(any(grepl("depositions", requestSummary(log))))
})

test_that("linkCollectionMembers() publishes the open edits of the members", {
  skip_if_not_installed("httr2")
  log = newLog()
  linked = list(list(identifier = "10.5072/zenodo.300", relation = "isPartOf", scheme = "doi"))
  httr2::local_mocked_responses(mockRouter(log, list(
    "GET /api/records/300/versions/latest$" = function(req) list(id = 301),
    "GET /api/deposit/depositions/301$" = function(req) {
      list(
        id = 301, submitted = TRUE, state = "done", conceptdoi = "10.5072/zenodo.300",
        metadata = list(related_identifiers = list(
          list(identifier = "10.5072/zenodo.201", relation = "hasPart", scheme = "doi"),
          list(identifier = "https://github.com/BirdScanCommunity/birdscanR", relation = "isCompiledBy", scheme = "url")
        ))
      )
    },
    "GET /api/records/201/versions/latest$" = function(req) list(id = 211),
    "GET /api/deposit/depositions/211$" = function(req) memberDeposition(211, state = "inprogress", related = linked),
    "POST /api/deposit/depositions/211/actions/publish$" = function(req) list(id = 211)
  )))
  expect_message(
    links <- linkCollectionMembers("10.5072/zenodo.300", token = "test-token", sandbox = TRUE, publish = TRUE),
    "published for 1"
  )
  expect_identical(links$added, 0L)
  expect_true(links$published)
  expect_identical(requestSummary(log)[length(log$requests)], "POST /api/deposit/depositions/211/actions/publish")
})

test_that(".zenodoConceptDoi() derives the concept DOI of drafts", {
  expect_identical(birdscanR:::.zenodoConceptDoi(list(conceptdoi = "10.5281/zenodo.5")), "10.5281/zenodo.5")
  expect_identical(
    birdscanR:::.zenodoConceptDoi(list(conceptrecid = "300", metadata = list(prereserve_doi = list(doi = "10.5072/zenodo.301")))),
    "10.5072/zenodo.300"
  )
  expect_null(birdscanR:::.zenodoConceptDoi(list(metadata = list())))
})

test_that("DOIs reserved on the Zenodo sandbox get the sandbox prefix", {
  expect_identical(birdscanR:::.zenodoReservedDoi("10.5281/zenodo.651401", sandbox = TRUE), "10.5072/zenodo.651401")
  expect_identical(birdscanR:::.zenodoReservedDoi("10.5281/zenodo.651401", sandbox = FALSE), "10.5281/zenodo.651401")
  expect_null(birdscanR:::.zenodoReservedDoi(NULL, sandbox = TRUE))
  # Drafts on the sandbox: concept DOI with the sandbox prefix
  draft = list(conceptrecid = "651400", doi = "", metadata = list(prereserve_doi = list(doi = "10.5281/zenodo.651401")))
  expect_identical(birdscanR:::.zenodoConceptDoi(draft, sandbox = TRUE), "10.5072/zenodo.651400")
})
