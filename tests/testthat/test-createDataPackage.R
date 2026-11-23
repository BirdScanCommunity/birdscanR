dbData = readRDS(system.file("extdata",
  "CH_Sempach_2024_SEP24_25_DataExtract.rds",
  package = "birdscanR"
))
metadataFile = system.file("extdata", "dataPackage", "metadataTemplate.yaml",
  package = "birdscanR"
)
metadataList = yaml::read_yaml(metadataFile)
resourceNames = c("site_data", "protocol_data", "echo_data", "blind_times", "sunrise_sunset")

test_that("createDataPackage() builds a package with all resources and schemas", {
  pkg = createDataPackage(dbExtract = dbData, metadata = metadataFile, pulseTypeSelection = "S")
  expect_s3_class(pkg, "mr1dp")
  expect_s3_class(pkg, "datapackage")
  expect_identical(frictionless::resources(pkg), resourceNames)
  expect_identical(pkg$name, "ch_sempach_2024")
  expect_identical(pkg$embargo, "2029-09-25")

  echo = frictionless::read_resource(pkg, "echo_data")
  expect_gt(nrow(echo), 0)
  expect_lte(nrow(echo), nrow(dbData$echoData))
  excludedEchoCols = c(
    "mtr_factor_old", "statistical_classification_old",
    "classification_id", "sphere_dia",
    "feature15.polRatio", "feature16.absPolarisation", "feature3.speed",
    "time_string", "time_bin", "altitude_bin"
  )
  expect_false(any(excludedEchoCols %in% names(echo)))
  expect_identical(ncol(echo), ncol(dbData$echoData) - length(excludedEchoCols))

  # Every field has a description; keys are set
  for (r in resourceNames) {
    schema = frictionless::get_schema(pkg, r)
    descriptions = vapply(schema$fields, function(f) f$description %||% NA_character_, character(1))
    expect_false(any(is.na(descriptions)), info = r)
    expect_false(any(descriptions == "No description available."), info = r)
  }
  expect_equal(as.character(frictionless::get_schema(pkg, "echo_data")$primaryKey), "echo")

  # Only short-pulse calibration columns in site_data
  site = frictionless::read_resource(pkg, "site_data")
  expect_true("short0V" %in% names(site))
  expect_false("medium0V" %in% names(site))
  expect_true("radarOrientation" %in% names(site))
  expect_true("blockTime" %in% names(frictionless::read_resource(pkg, "protocol_data")))
})

test_that("createDataPackage() works without pulse type selection and with several pulse types", {
  pkgAll = createDataPackage(dbExtract = dbData, metadata = metadataList)
  pkgSM = createDataPackage(dbExtract = dbData, metadata = metadataList, pulseTypeSelection = c("S", "M"))
  expect_identical(
    nrow(frictionless::read_resource(pkgAll, "echo_data")),
    nrow(dbData$echoData)
  )
  expect_true(all(c("short0V", "medium0V") %in% names(frictionless::read_resource(pkgSM, "site_data"))))
})

test_that("createDataPackage() writes a package that can be read back with intact keys", {
  outDir = withr::local_tempdir()
  pkg = createDataPackage(
    dbExtract = dbData, metadata = metadataFile,
    timeRangeTargetTZ = c("2024-09-24 00:00", "2024-09-25 23:59"),
    outputDirPath = outDir
  )
  pkgRead = frictionless::read_package(file.path(outDir, "CH_Sempach_2024", "datapackage.json"))
  expect_identical(frictionless::resources(pkgRead), resourceNames)
  # JSON arrays are read back as vectors or lists, depending on the platform
  expect_identical(unlist(pkgRead$contributors[[1]]$roles), c("ContactPerson", "DataCurator"))
  expect_equal(unlist(pkgRead$spatial$coordinates), c(dbData$siteData$longitude, dbData$siteData$latitude))

  echo = frictionless::read_resource(pkgRead, "echo_data")
  protocol = frictionless::read_resource(pkgRead, "protocol_data")
  site = frictionless::read_resource(pkgRead, "site_data")
  blind = frictionless::read_resource(pkgRead, "blind_times")
  expect_false(anyDuplicated(echo$echo) > 0)
  expect_true(all(echo$protocolID %in% protocol$protocolID))
  expect_true(all(protocol$siteID %in% site$siteID))
  expect_true(all(stats::na.omit(blind$protocolID) %in% protocol$protocolID))
  expect_true(all(c("protocolChange", "visibility") %in% blind$type))
})

test_that("createDataPackage() merges manual blind times", {
  # The example data(manualBlindTimes) fall within the period of the test data
  utils::data("manualBlindTimes", package = "birdscanR", envir = environment())
  tmpFile = withr::local_tempfile(fileext = ".csv")
  utils::write.table(manualBlindTimes, file = tmpFile, sep = ",", row.names = FALSE, col.names = FALSE)
  cManualBlindTimes = expect_no_warning(
    loadManualBlindTimes(filePath = tmpFile, blindTimesTZ = "Etc/GMT0", targetTZ = "Etc/GMT0")
  )
  pkg = expect_no_warning(
    createDataPackage(dbExtract = dbData, metadata = metadataList, manualBlindTimes = cManualBlindTimes)
  )
  blind = frictionless::read_resource(pkg, "blind_times")
  expect_true(all(c("rain", "tech", "clutter") %in% blind$type))
  expect_true(pkg$`mr1dp:filterParameters`$manualBlindTimes)
})

test_that("createDataPackage() rejects invalid metadata with a clear message", {
  bad = metadataList
  bad$title = NULL
  bad$contributors[[1]]$roles = list("Author")
  bad$contributors[[1]]$path = "https://orcid.org/0000-0002-8739-6461"
  bad$contributors[[1]]$embargoRequestType = 3
  err = expect_error(createDataPackage(dbExtract = dbData, metadata = bad))
  expect_match(conditionMessage(err), "'title' is required")
  expect_match(conditionMessage(err), "invalid role\\(s\\) Author")
  expect_match(conditionMessage(err), "valid ORCID")
  expect_match(conditionMessage(err), "embargoRequestType")
  expect_match(conditionMessage(err), "ContactPerson")
})

test_that("createDataPackageMetadataTemplate() writes a valid template", {
  outFile = withr::local_tempfile(fileext = ".yaml")
  createDataPackageMetadataTemplate(outFile)
  expect_true(file.exists(outFile))
  expect_length(birdscanR:::.checkPackageMetadata(yaml::read_yaml(outFile)), 0)
  expect_error(createDataPackageMetadataTemplate(outFile), "already exists")
})
