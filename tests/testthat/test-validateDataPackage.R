dbData = readRDS(system.file("extdata",
  "CH_Sempach_2024_SEP24_25_DataExtract.rds",
  package = "birdscanR"
))
metadataFile = system.file("extdata", "dataPackage", "metadataTemplate.yaml",
  package = "birdscanR"
)
pkg = createDataPackage(dbExtract = dbData, metadata = metadataFile, pulseTypeSelection = "S")

# Replace the data of a resource of an in-memory data package
setResourceData = function(package, resourceName, data) {
  i = which(frictionless::resources(package) == resourceName)
  package$resources[[i]]$data = data
  package
}

test_that("validateDataPackage() accepts a package created by createDataPackage()", {
  validation = validateDataPackage(pkg)
  expect_s3_class(validation, "mr1dpValidation")
  expect_true(validation$valid)
  expect_identical(nrow(validation$issues), 0L)
  expect_output(print(validation), "is valid")

  pkgAll = createDataPackage(dbExtract = dbData, metadata = metadataFile)
  expect_true(validateDataPackage(pkgAll)$valid)
})

test_that("validateDataPackage() validates a package on disk, by directory or json path", {
  outDir = withr::local_tempdir()
  createDataPackage(dbExtract = dbData, metadata = metadataFile, outputDirPath = outDir)
  packageDir = file.path(outDir, "CH_Sempach_2024")
  expect_true(validateDataPackage(packageDir)$valid)
  expect_true(validateDataPackage(file.path(packageDir, "datapackage.json"))$valid)
  expect_error(validateDataPackage(file.path(outDir, "doesNotExist")), "No data package found")
})

test_that("validateDataPackage() reports values that do not match the field type on disk", {
  outDir = withr::local_tempdir()
  createDataPackage(dbExtract = dbData, metadata = metadataFile, outputDirPath = outDir)
  packageDir = file.path(outDir, "CH_Sempach_2024")
  protocolFile = file.path(packageDir, "protocol_data.csv.gz")
  protocol = utils::read.csv(protocolFile, colClasses = "character")
  protocol$stc[1] = "not a number"
  utils::write.csv(protocol, gzfile(protocolFile), row.names = FALSE, na = "")

  validation = validateDataPackage(packageDir)
  expect_false(validation$valid)
  expect_true(any(validation$issues$resource == "protocol_data" &
    validation$issues$field == "stc" & validation$issues$level == "error"))
})

test_that("validateDataPackage() reports missing resources and columns", {
  bad = frictionless::remove_resource(pkg, "blind_times")
  validation = validateDataPackage(bad)
  expect_false(validation$valid)
  expect_true(any(validation$issues$resource == "blind_times" &
    grepl("Required resource is missing", validation$issues$message)))

  site = frictionless::read_resource(pkg, "site_data")
  site$altitude = NULL
  bad = setResourceData(pkg, "site_data", site)
  validation = validateDataPackage(bad)
  expect_false(validation$valid)
  expect_true(any(validation$issues$field == "altitude" &
    grepl("Required column is missing", validation$issues$message)))
  expect_true(any(grepl("do not match the field names", validation$issues$message)))
})

test_that("validateDataPackage() reports wrong types and broken keys", {
  echo = frictionless::read_resource(pkg, "echo_data")
  echo$feature1.altitude_AGL = as.character(echo$feature1.altitude_AGL)
  echo$echo[2] = echo$echo[1]
  echo$protocolID[3] = 999999L
  bad = setResourceData(pkg, "echo_data", echo)
  issues = validateDataPackage(bad)$issues

  expect_true(any(issues$field == "feature1.altitude_AGL" & grepl("field type 'number'", issues$message)))
  expect_true(any(issues$field == "echo" & grepl("1 row\\(s\\) have a duplicated primary key", issues$message)))
  expect_true(any(issues$field == "protocolID" & grepl("1 row\\(s\\) refer to a value that does not exist in 'protocol_data'", issues$message)))
})

test_that("validateDataPackage() reports implausible content", {
  protocol = frictionless::read_resource(pkg, "protocol_data")
  protocol$stopTime_targetTZ[1] = protocol$startTime_targetTZ[1] - 60
  bad = setResourceData(pkg, "protocol_data", protocol)
  bad$temporal$end = "2024-09-24"
  bad$spatial$coordinates = I(c(0, 0))
  issues = validateDataPackage(bad)$issues

  expect_true(any(issues$resource == "protocol_data" & grepl("is not before", issues$message)))
  expect_true(any(issues$level == "warning" & grepl("outside the period of their measurement period", issues$message)))
  expect_true(any(grepl("outside the temporal coverage", issues$message)))
  expect_true(any(grepl("site coordinates differ", issues$message)))
})

test_that("validateDataPackage() reports invalid metadata, and warnings do not invalidate", {
  bad = pkg
  bad$title = NULL
  bad$contributors[[1]]$roles = I("DataCurator")
  validation = validateDataPackage(bad)
  expect_false(validation$valid)
  expect_true(any(grepl("'title' is required", validation$issues$message)))
  expect_true(any(grepl("ContactPerson", validation$issues$message)))
  expect_output(print(validation), "NOT valid")

  warn = pkg
  i = which(frictionless::resources(warn) == "site_data")
  warn$resources[[i]]$schema$fields[[1]]$description = NULL
  warn$`mr1dp:provenance` = NULL
  blind = frictionless::read_resource(warn, "blind_times")
  blind$protocolID[1] = NA
  warn = setResourceData(warn, "blind_times", blind)
  validation = validateDataPackage(warn)
  expect_true(validation$valid)
  expect_identical(sum(validation$issues$level == "warning"), 3L)
  expect_true(any(grepl("not linked to a measurement period", validation$issues$message)))
})
