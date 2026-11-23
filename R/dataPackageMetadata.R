#' @title Create MR1 Data Package Metadata Template
#' @author Birgen Haest
#' @description [createDataPackageMetadataTemplate()] writes a commented YAML
#' template with the package-level metadata of an MR1 data package: campaign ID,
#' title, description, version, owner, licence, embargo, keywords, contributors
#' (with ORCID, affiliation, DataCite roles and embargo request type), related
#' identifiers and suggested citation. Fill in the template and pass its path to
#' the `metadata` argument of [createDataPackage()].
#' @param filePath Path of the YAML file to write.
#' @param overwrite logical. If `TRUE`, an existing file is overwritten. Default
#' is `FALSE`.
#'
#' @return The path of the written file, invisibly.
#' @family write file functions
#' @export
#' @examples
#' # Write the metadata template
#' # ===========================================================================
#' metadataFile = file.path(tempdir(), "CH_Sempach_2024_metadata.yaml")
#' createDataPackageMetadataTemplate(filePath = metadataFile, overwrite = TRUE)
createDataPackageMetadataTemplate = function(filePath, overwrite = FALSE) {
  if (!is.character(filePath) || length(filePath) != 1) {
    stop("'filePath' must be a single character string.")
  }
  if (file.exists(filePath) && !isTRUE(overwrite)) {
    stop(paste0("'filePath' already exists: ", filePath, ". Use 'overwrite = TRUE' to replace it."))
  }
  templatePath = system.file("extdata", "dataPackage", "metadataTemplate.yaml",
    package = "birdscanR"
  )
  file.copy(templatePath, filePath, overwrite = TRUE)
  invisible(filePath)
}

# Allowed values for package-level metadata
# ==============================================================================
.mr1dpRoles = c(
  "ContactPerson", "ProjectLeader", "DataCollector", "DataCurator",
  "Researcher", "RightsHolder", "Supervisor"
)
.mr1dpLicenses = c(
  "CC-BY-4.0"    = "https://creativecommons.org/licenses/by/4.0/",
  "CC-BY-NC-4.0" = "https://creativecommons.org/licenses/by-nc/4.0/"
)
.mr1dpDefaultEmbargoYears = 5

#' Read MR1 data package metadata from a YAML file or a list
#' @param metadata path to a YAML file, or a named list with the same structure.
#' @return named list
#' @noRd
.readPackageMetadata = function(metadata) {
  if (is.character(metadata) && length(metadata) == 1) {
    if (!file.exists(metadata)) {
      stop(paste0("The metadata file does not exist: ", metadata))
    }
    md = yaml::read_yaml(metadata)
  } else if (is.list(metadata)) {
    md = metadata
  } else {
    stop("'metadata' must be the path to a YAML file or a named list. See createDataPackageMetadataTemplate().")
  }
  if (is.null(names(md)) || any(names(md) == "")) {
    stop("'metadata' must be a named list.")
  }
  return(md)
}

#' Check whether an ORCID has a valid format and checksum (ISO 7064 11,2)
#' @param orcid ORCID, either bare (0000-0000-0000-0000) or as URL
#' @return logical
#' @noRd
.isValidOrcid = function(orcid) {
  id = sub("^https?://orcid.org/", "", orcid)
  if (!grepl("^\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9X]$", id)) {
    return(FALSE)
  }
  digits = strsplit(gsub("-", "", id), "")[[1]]
  total = 0
  for (d in digits[1:15]) {
    total = (total + as.integer(d)) * 2
  }
  check = (12 - (total %% 11)) %% 11
  check = ifelse(check == 10, "X", as.character(check))
  return(check == digits[16])
}

#' Check MR1 data package metadata
#' @description Returns the problems found instead of stopping, so that the
#' same checks can be reused by a data package validator.
#' @param md named list with package metadata, see `.readPackageMetadata()`.
#' @return character vector with problems; empty if none
#' @noRd
.checkPackageMetadata = function(md) {
  problems = character(0)
  isString = function(x) is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)

  # Required single strings
  # ============================================================================
  for (field in c("campaignID", "title", "description", "version")) {
    if (!isString(md[[field]])) {
      problems = c(problems, paste0("'", field, "' is required and must be a single, non-empty string."))
    }
  }
  if (isString(md$campaignID) && !grepl("^[A-Za-z0-9._-]+$", md$campaignID)) {
    problems = c(problems, "'campaignID' may only contain letters, digits, '.', '-' and '_'.")
  }

  # Owner
  # ============================================================================
  if (!is.list(md$owner) || !isString(md$owner$name)) {
    problems = c(problems, "'owner' is required and must have a 'name'.")
  }

  # Licence
  # ============================================================================
  if (!is.list(md$licenses) || length(md$licenses) == 0) {
    problems = c(problems, "'licenses' is required and must contain at least one licence.")
  } else {
    for (lic in md$licenses) {
      if (!is.list(lic) || !isString(lic$name) || !lic$name %in% names(.mr1dpLicenses)) {
        problems = c(problems, paste0(
          "Each licence must have a 'name' out of: ",
          paste(names(.mr1dpLicenses), collapse = ", "), "."
        ))
      }
    }
  }

  # Embargo
  # ============================================================================
  if (!is.null(md$embargo)) {
    embargoDate = tryCatch(as.Date(md$embargo), error = function(e) NA)
    if (length(embargoDate) != 1 || is.na(embargoDate)) {
      problems = c(problems, "'embargo' must be a date formatted as YYYY-MM-DD, or empty.")
    }
  }

  # Contributors
  # ============================================================================
  if (!is.list(md$contributors) || length(md$contributors) == 0) {
    problems = c(problems, "'contributors' is required and must contain at least one contributor.")
  } else {
    allRoles = character(0)
    for (i in seq_along(md$contributors)) {
      ctb = md$contributors[[i]]
      label = paste0("Contributor ", i, if (is.list(ctb) && isString(ctb$title)) paste0(" (", ctb$title, ")"))
      if (!is.list(ctb) || !isString(ctb$title)) {
        problems = c(problems, paste0(label, ": 'title' (full name) is required."))
        next
      }
      roles = unlist(ctb$roles)
      if (length(roles) == 0) {
        problems = c(problems, paste0(label, ": at least one role is required."))
      } else if (!all(roles %in% .mr1dpRoles)) {
        problems = c(problems, paste0(
          label, ": invalid role(s) ",
          paste(setdiff(roles, .mr1dpRoles), collapse = ", "),
          ". Allowed: ", paste(.mr1dpRoles, collapse = ", "), "."
        ))
      }
      allRoles = c(allRoles, roles)
      if (!is.null(ctb$path) && !.isValidOrcid(ctb$path)) {
        problems = c(problems, paste0(label, ": 'path' must be a valid ORCID URL (https://orcid.org/0000-0000-0000-0000)."))
      }
      if (is.null(ctb$embargoRequestType) || !ctb$embargoRequestType %in% c(1, 2)) {
        problems = c(problems, paste0(label, ": 'embargoRequestType' must be 1 (contact me) or 2 (citation only)."))
      }
    }
    if (!"ContactPerson" %in% allRoles) {
      problems = c(problems, "At least one contributor must have the role 'ContactPerson'.")
    }
  }

  # Related identifiers
  # ============================================================================
  for (ri in md$relatedIdentifiers) {
    if (!is.list(ri) || !isString(ri$relationType) || !isString(ri$relatedIdentifier) ||
      !isString(ri$relatedIdentifierType)) {
      problems = c(problems, "Each related identifier needs 'relationType', 'relatedIdentifier' and 'relatedIdentifierType'.")
    }
  }

  return(unique(problems))
}

#' Build the suggested citation of an MR1 data package
#' @param md named list with package metadata
#' @param year year of publication
#' @return character string
#' @noRd
.buildCitation = function(md, year) {
  authors = vapply(md$contributors, function(ctb) {
    if (!is.null(ctb$familyName) && !is.null(ctb$givenName)) {
      initials = paste0(substr(strsplit(ctb$givenName, "[ -]")[[1]], 1, 1), collapse = "")
      paste(ctb$familyName, initials)
    } else {
      ctb$title
    }
  }, character(1))
  citation = paste0(
    paste(authors, collapse = ", "), " (", year, ") ", md$title,
    ". Version ", md$version, "."
  )
  if (!is.null(md$id)) citation = paste(citation, md$id)
  return(citation)
}

#' Convert a vector to a JSON array, also when it has length 1
#' @noRd
.jsonArray = function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  return(I(x))
}

#' Complete MR1 data package metadata with properties derived from the data
#' @param md named list with checked package metadata
#' @param siteData site table of the package
#' @param echoData echo table of the package
#' @param startTime,stopTime POSIXct, time window of the package
#' @param timeZone data frame with radarTimeZone and targetTimeZone
#' @param filterParameters named list with the applied filters
#' @param dbName name of the database
#' @return named list with the package-level properties
#' @noRd
.completePackageMetadata = function(md, siteData, echoData, startTime, stopTime,
                                    timeZone, filterParameters, dbName) {
  # Temporal coverage and embargo
  # ============================================================================
  startDate = as.Date(startTime, tz = timeZone$targetTimeZone)
  endDate = as.Date(stopTime, tz = timeZone$targetTimeZone)
  if (is.null(md$embargo)) {
    embargo = seq(endDate, by = paste(.mr1dpDefaultEmbargoYears, "years"), length.out = 2)[2]
  } else {
    embargo = as.Date(md$embargo)
  }

  # Licences: fill in the URL of known licences
  # ============================================================================
  licenses = lapply(md$licenses, function(lic) {
    lic$path = lic$path %||% unname(.mr1dpLicenses[lic$name])
    lic
  })

  # Contributors: make sure roles are written as JSON arrays
  # ============================================================================
  contributors = lapply(md$contributors, function(ctb) {
    ctb$roles = .jsonArray(unlist(ctb$roles))
    ctb
  })

  # Package-level properties (Frictionless names where available)
  # ============================================================================
  created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  out = list(
    name                  = tolower(md$campaignID),
    id                    = md$id,
    title                 = md$title,
    description           = md$description,
    version               = md$version,
    created               = created,
    campaignID            = md$campaignID,
    owner                 = md$owner,
    licenses              = licenses,
    embargo               = format(embargo, "%Y-%m-%d"),
    keywords              = .jsonArray(unlist(md$keywords)),
    contributors          = contributors,
    relatedIdentifiers    = md$relatedIdentifiers,
    bibliographicCitation = md$bibliographicCitation %||%
      .buildCitation(md, format(Sys.Date(), "%Y")),
    spatial = list(
      type        = "Point",
      coordinates = .jsonArray(c(siteData$longitude[1], siteData$latitude[1]))
    ),
    temporal = list(
      start = format(startDate, "%Y-%m-%d"),
      end   = format(endDate, "%Y-%m-%d")
    ),
    site = list(
      siteID    = siteData$siteID[1],
      siteCode  = siteData$siteCode[1],
      siteName  = siteData$siteName[1],
      latitude  = siteData$latitude[1],
      longitude = siteData$longitude[1],
      altitude  = siteData$altitude[1]
    ),
    "mr1dp:filterParameters" = filterParameters,
    "mr1dp:provenance" = list(
      database             = dbName,
      birdscanR            = as.character(utils::packageVersion("birdscanR")),
      radarType            = siteData$type[1],
      radarSerialNo        = siteData$serialNo[1],
      radarTimeZone        = timeZone$radarTimeZone,
      targetTimeZone       = timeZone$targetTimeZone,
      classifierVersion    = .jsonArray(sort(unique(stats::na.omit(echoData$classifierVersion)))),
      batClassifierVersion = .jsonArray(sort(unique(stats::na.omit(echoData$batClassifierVersion))))
    )
  )

  # Drop empty properties (NULL), keep the order
  # ============================================================================
  out = out[!vapply(out, is.null, logical(1))]
  return(out)
}
