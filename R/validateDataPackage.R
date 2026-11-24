#' @title Validate MR1 Data Package
#' @author Birgen Haest
#' @description [validateDataPackage()] checks whether an MR1 data package,
#' created by [createDataPackage()], is complete and consistent before it is
#' published, e.g. on Zenodo. It checks:
#' * the package structure (Frictionless Data Package) and the package-level
#'   metadata (the same checks as in [createDataPackage()], plus the
#'   properties that [createDataPackage()] adds automatically);
#' * that all five resources (`echo_data`, `protocol_data`, `blind_times`,
#'   `sunrise_sunset`, `site_data`) are present, can be read, and contain
#'   the required columns;
#' * that the column names and types match the Table Schema of each resource,
#'   and that every column has a description;
#' * that primary keys are unique and not missing, and that foreign keys
#'   (e.g. `protocolID` of the echoes) refer to existing rows;
#' * that the content is plausible: start times before stop times, echoes
#'   within their measurement period and within the temporal coverage, and
#'   site coordinates equal to the spatial coverage.
#'
#' Problems that make the package unusable or inconsistent are reported as
#' errors; problems that do not, e.g. a column without description, as
#' warnings. A package is valid when there are no errors.
#' @param package MR1 data package object returned by [createDataPackage()] or
#' [frictionless::read_package()], or the path to a `datapackage.json` file or
#' to the directory that contains it.
#'
#' @return An object of class `mr1dpValidation`: a list with `valid` (logical,
#' `TRUE` if no errors were found) and `issues` (data frame with the columns
#' `level` (`"error"` or `"warning"`), `resource`, `field` and `message`).
#' Printing it gives a summary of the issues.
#' @family write file functions
#' @export
#' @examples
#' \donttest{
#' # Load example data
#' # ===========================================================================
#' dbData = readRDS(system.file("extdata",
#'   "CH_Sempach_2024_SEP24_25_DataExtract.rds",
#'   package = "birdscanR"
#' ))
#' mainOutputDir = tempdir()
#'
#' # Write the package metadata template (fill it in for a real campaign)
#' # ===========================================================================
#' metadataFile = file.path(mainOutputDir, "CH_Sempach_2024_metadata.yaml")
#' createDataPackageMetadataTemplate(filePath = metadataFile, overwrite = TRUE)
#'
#' # Create MR1 data package
#' # ===========================================================================
#' dataPackage = createDataPackage(
#'   dbExtract          = dbData,
#'   metadata           = metadataFile,
#'   pulseTypeSelection = "S",
#'   outputDirPath      = mainOutputDir
#' )
#'
#' # Validate the data package object
#' # ===========================================================================
#' validateDataPackage(dataPackage)
#'
#' # Validate the data package files on disk
#' # ===========================================================================
#' validation = validateDataPackage(file.path(mainOutputDir, "CH_Sempach_2024"))
#' validation$valid
#' }
validateDataPackage = function(package) {
  issues = .newIssues()
  addIssue = function(level, message, resource = NA_character_, field = NA_character_) {
    issues <<- rbind(issues, data.frame(
      level = level, resource = resource, field = field, message = message,
      stringsAsFactors = FALSE
    ))
  }

  # Read the package, if a path is given
  # ==============================================================================
  if (is.character(package) && length(package) == 1) {
    packagePath = package
    if (dir.exists(packagePath)) packagePath = file.path(packagePath, "datapackage.json")
    if (!file.exists(packagePath)) {
      stop(paste0("No data package found at: ", package))
    }
    package = tryCatch(
      suppressMessages(frictionless::read_package(packagePath)),
      error = function(e) {
        addIssue("error", paste("The data package could not be read:", conditionMessage(e)))
        NULL
      }
    )
    if (is.null(package)) {
      return(.newValidation(issues))
    }
  } else if (!is.list(package)) {
    stop("'package' must be a data package object or the path to a 'datapackage.json' file or its directory.")
  }

  # Package structure
  # ==============================================================================
  structureOk = tryCatch(
    {
      frictionless::check_package(package)
      TRUE
    },
    error = function(e) {
      addIssue("error", paste("Invalid Frictionless Data Package:", conditionMessage(e)))
      FALSE
    },
    warning = function(w) {
      addIssue("warning", conditionMessage(w))
      TRUE
    }
  )
  if (!structureOk) {
    return(.newValidation(issues))
  }

  # Package-level metadata
  # ==============================================================================
  for (problem in .checkPackageMetadata(package)) addIssue("error", problem)
  if (is.null(package$name) || !grepl("^[a-z0-9._-]+$", package$name)) {
    addIssue("error", "'name' is required and may only contain lower case letters, digits, '.', '-' and '_'.")
  } else if (!is.null(package$campaignID) && !identical(package$name, tolower(package$campaignID))) {
    addIssue("warning", "'name' is not the lower case 'campaignID'.")
  }
  if (is.null(package$embargo)) {
    addIssue("error", "'embargo' is missing.")
  }
  temporalStart = .asDateOrNA(package$temporal$start)
  temporalEnd = .asDateOrNA(package$temporal$end)
  if (is.na(temporalStart) || is.na(temporalEnd)) {
    addIssue("error", "'temporal' must have a 'start' and 'end' date formatted as YYYY-MM-DD.")
  } else if (temporalStart > temporalEnd) {
    addIssue("error", "The start of 'temporal' is after its end.")
  }
  coordinates = suppressWarnings(as.numeric(unlist(package$spatial$coordinates)))
  if (length(coordinates) != 2 || any(is.na(coordinates))) {
    addIssue("error", "'spatial' must have 'coordinates' with longitude and latitude.")
    coordinates = NULL
  }
  for (prop in c("bibliographicCitation", "mr1dp:filterParameters", "mr1dp:provenance")) {
    if (is.null(package[[prop]])) addIssue("warning", paste0("'", prop, "' is missing."))
  }
  targetTZ = package$`mr1dp:provenance`$targetTimeZone %||% "UTC"

  # Resources: presence
  # ==============================================================================
  presentResources = frictionless::resources(package)
  missingResources = setdiff(names(.mr1dpKeys), presentResources)
  for (r in missingResources) {
    addIssue("error", "Required resource is missing.", resource = r)
  }
  for (r in setdiff(presentResources, names(.mr1dpKeys))) {
    addIssue("warning", "Unexpected resource; it is not part of the MR1 data package standard.", resource = r)
  }

  # Resources: read data, check columns against schema and required columns
  # ==============================================================================
  data = list()
  for (r in intersect(names(.mr1dpKeys), presentResources)) {
    schema = tryCatch(frictionless::get_schema(package, r), error = function(e) {
      addIssue("error", paste("Invalid schema:", conditionMessage(e)), resource = r)
      NULL
    })
    if (is.null(schema)) next

    readWarnings = character(0)
    df = withCallingHandlers(
      tryCatch(frictionless::read_resource(package, r), error = function(e) {
        addIssue("error", paste("The data could not be read:", conditionMessage(e)), resource = r)
        NULL
      }),
      warning = function(w) {
        readWarnings <<- c(readWarnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    if (is.null(df)) next
    for (w in unique(readWarnings)) addIssue("error", paste("Reading the data gave a warning:", w), resource = r)
    parseProblems = tryCatch(frictionless::problems(df), error = function(e) NULL)
    if (is.data.frame(parseProblems) && nrow(parseProblems) > 0) {
      for (col in unique(parseProblems$col)) {
        n = sum(parseProblems$col == col)
        addIssue("error", paste0(
          n, " value(s) do not match the field type, e.g. '",
          parseProblems$actual[parseProblems$col == col][1], "' (expected ",
          parseProblems$expected[parseProblems$col == col][1], ")."
        ), resource = r, field = names(df)[col] %||% as.character(col))
      }
    }
    data[[r]] = df

    fieldNames = vapply(schema$fields, function(f) f$name %||% NA_character_, character(1))
    if (!identical(fieldNames, names(df))) {
      addIssue("error", "The column names of the data do not match the field names of the schema.", resource = r)
    }
    for (col in setdiff(.mr1dpRequiredFields[[r]], names(df))) {
      addIssue("error", "Required column is missing.", resource = r, field = col)
    }
    for (f in schema$fields) {
      if (is.null(f$description) || !nzchar(f$description) || f$description == "No description available.") {
        addIssue("warning", "The field has no description.", resource = r, field = f$name)
      }
      if (f$name %in% names(df) && !.fieldTypeMatches(df[[f$name]], f$type)) {
        addIssue("error", paste0(
          "The values (", class(df[[f$name]])[1], ") do not match the field type '", f$type, "'."
        ), resource = r, field = f$name)
      }
    }
    if (nrow(df) == 0 && r %in% c("echo_data", "protocol_data", "site_data")) {
      addIssue("error", "The resource has no rows.", resource = r)
    }

    # Primary key
    # ============================================================================
    pk = unlist(schema$primaryKey)
    if (!is.null(pk) && all(pk %in% names(df))) {
      keyValues = df[, pk, drop = FALSE]
      nMissing = sum(!stats::complete.cases(keyValues))
      if (nMissing > 0) {
        addIssue("error", paste(nMissing, "row(s) have a missing primary key."), resource = r, field = paste(pk, collapse = ", "))
      }
      nDuplicated = sum(duplicated(keyValues))
      if (nDuplicated > 0) {
        addIssue("error", paste(nDuplicated, "row(s) have a duplicated primary key."), resource = r, field = paste(pk, collapse = ", "))
      }
    } else if (!is.null(.mr1dpKeys[[r]]$primaryKey)) {
      addIssue("error", paste0("The schema has no primary key '", .mr1dpKeys[[r]]$primaryKey, "'."), resource = r)
    }
  }

  # Foreign keys
  # ==============================================================================
  for (r in names(data)) {
    schema = frictionless::get_schema(package, r)
    for (fk in schema$foreignKeys) {
      fields = unlist(fk$fields)
      refResource = fk$reference$resource
      refFields = unlist(fk$reference$fields)
      if (is.null(data[[refResource]]) || !all(fields %in% names(data[[r]])) ||
        !all(refFields %in% names(data[[refResource]]))) {
        addIssue("error", paste0("The foreign key refers to a resource or field that is not available: ", refResource, "."),
          resource = r, field = paste(fields, collapse = ", ")
        )
        next
      }
      values = data[[r]][, fields, drop = FALSE]
      values = values[stats::complete.cases(values), , drop = FALSE]
      refKeys = do.call(paste, c(data[[refResource]][, refFields, drop = FALSE], sep = "\r"))
      nUnmatched = sum(!do.call(paste, c(values, sep = "\r")) %in% refKeys)
      if (nUnmatched > 0) {
        addIssue("error", paste0(nUnmatched, " row(s) refer to a value that does not exist in '", refResource, "'."),
          resource = r, field = paste(fields, collapse = ", ")
        )
      }
    }
  }
  if (!is.null(data$blind_times) && "protocolID" %in% names(data$blind_times)) {
    nNoProtocol = sum(is.na(data$blind_times$protocolID))
    if (nNoProtocol > 0) {
      addIssue("warning", paste(nNoProtocol, "blind time(s) are not linked to a measurement period (protocol)."),
        resource = "blind_times", field = "protocolID"
      )
    }
  }

  # Plausibility of the content
  # ==============================================================================
  checkStartStop = function(r, startCol, stopCol) {
    df = data[[r]]
    if (is.null(df) || !all(c(startCol, stopCol) %in% names(df))) {
      return()
    }
    nWrong = sum(df[[startCol]] >= df[[stopCol]], na.rm = TRUE)
    if (nWrong > 0) {
      addIssue("error", paste0(nWrong, " row(s) have a '", startCol, "' that is not before '", stopCol, "'."), resource = r)
    }
  }
  checkStartStop("protocol_data", "startTime_targetTZ", "stopTime_targetTZ")
  checkStartStop("blind_times", "start_targetTZ", "stop_targetTZ")
  checkStartStop("sunrise_sunset", "sunStart", "sunStop")

  echo = data$echo_data
  if (!is.null(echo) && "time_stamp_targetTZ" %in% names(echo) && inherits(echo$time_stamp_targetTZ, "POSIXt")) {
    # Echoes within the temporal coverage
    # ============================================================================
    if (!is.na(temporalStart) && !is.na(temporalEnd)) {
      echoDates = as.Date(echo$time_stamp_targetTZ, tz = targetTZ)
      nOutside = sum(echoDates < temporalStart | echoDates > temporalEnd, na.rm = TRUE)
      if (nOutside > 0) {
        addIssue("error", paste(nOutside, "echo(es) fall outside the temporal coverage of the package."),
          resource = "echo_data", field = "time_stamp_targetTZ"
        )
      }
    }

    # Echoes within their measurement period
    # ============================================================================
    protocol = data$protocol_data
    if (!is.null(protocol) && "protocolID" %in% names(echo) &&
      all(c("protocolID", "startTime_targetTZ", "stopTime_targetTZ") %in% names(protocol))) {
      ind = match(echo$protocolID, protocol$protocolID)
      outside = !is.na(ind) &
        (echo$time_stamp_targetTZ < protocol$startTime_targetTZ[ind] |
          echo$time_stamp_targetTZ > protocol$stopTime_targetTZ[ind])
      nOutside = sum(outside, na.rm = TRUE)
      if (nOutside > 0) {
        addIssue("warning", paste(nOutside, "echo(es) fall outside the period of their measurement period (protocol)."),
          resource = "echo_data", field = "time_stamp_targetTZ"
        )
      }
    }
  }

  # Site coordinates equal to the spatial coverage
  # ============================================================================
  site = data$site_data
  if (!is.null(site) && !is.null(coordinates) && all(c("longitude", "latitude") %in% names(site)) && nrow(site) > 0) {
    if (!isTRUE(all.equal(c(site$longitude[1], site$latitude[1]), coordinates, tolerance = 1e-6, check.attributes = FALSE))) {
      addIssue("error", "The site coordinates differ from the 'spatial' coordinates of the package.", resource = "site_data")
    }
    if (nrow(site) > 1) {
      addIssue("warning", "The package contains more than one site; an MR1 data package covers one radar campaign.", resource = "site_data")
    }
  }

  return(.newValidation(issues))
}

#' Required columns of the MR1 data package resources
#' @noRd
.mr1dpRequiredFields = list(
  site_data = c("siteID", "latitude", "longitude", "altitude"),
  protocol_data = c("protocolID", "siteID", "startTime_targetTZ", "stopTime_targetTZ", "pulseType", "rotate"),
  echo_data = c(
    "echo", "protocolID", "time_stamp_targetTZ", "feature1.altitude_AGL",
    "class", "class_probability", "mtr_factor_rf"
  ),
  blind_times = c("type", "start_targetTZ", "stop_targetTZ", "protocolID"),
  sunrise_sunset = c("date", "sunStart", "sunStop")
)

#' Empty issues data frame of a data package validation
#' @noRd
.newIssues = function() {
  data.frame(
    level = character(0), resource = character(0), field = character(0),
    message = character(0), stringsAsFactors = FALSE
  )
}

#' Create a data package validation result
#' @noRd
.newValidation = function(issues) {
  rownames(issues) = NULL
  structure(
    list(valid = !any(issues$level == "error"), issues = issues),
    class = "mr1dpValidation"
  )
}

#' Convert to Date, or NA if not possible
#' @noRd
.asDateOrNA = function(x) {
  if (is.null(x) || length(x) != 1) {
    return(as.Date(NA))
  }
  tryCatch(as.Date(as.character(x), format = "%Y-%m-%d"), error = function(e) as.Date(NA))
}

#' Check whether the values of a column match a Table Schema field type
#' @noRd
.fieldTypeMatches = function(x, type) {
  # Columns without values match any type (create_schema() types them as string)
  if (is.null(type) || all(is.na(x))) {
    return(TRUE)
  }
  switch(type,
    number = is.numeric(x),
    integer = is.numeric(x) && all(is.na(x) | x == round(x)),
    string = is.character(x) || is.factor(x),
    boolean = is.logical(x),
    date = inherits(x, "Date"),
    datetime = inherits(x, "POSIXt"),
    time = inherits(x, c("hms", "difftime")),
    TRUE
  )
}

#' Print an MR1 data package validation
#' @param x object of class `mr1dpValidation`, returned by
#' [validateDataPackage()].
#' @param ... not used.
#' @return `x`, invisibly.
#' @export
#' @keywords internal
print.mr1dpValidation = function(x, ...) {
  nErrors = sum(x$issues$level == "error")
  nWarnings = sum(x$issues$level == "warning")
  if (x$valid) {
    cat("The MR1 data package is valid")
    cat(if (nWarnings > 0) paste0(", with ", nWarnings, " warning(s).\n") else ".\n")
  } else {
    cat("The MR1 data package is NOT valid:", nErrors, "error(s) and", nWarnings, "warning(s).\n")
  }
  for (i in seq_len(nrow(x$issues))) {
    issue = x$issues[i, ]
    where = paste(stats::na.omit(c(issue$resource, issue$field)), collapse = "$")
    cat(
      "- [", issue$level, "] ", if (nzchar(where)) paste0(where, ": "), issue$message, "\n",
      sep = ""
    )
  }
  invisible(x)
}
