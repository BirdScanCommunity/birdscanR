#' @title Upload MR1 Data Package to Zenodo
#' @author Birgen Haest
#' @description [uploadDataPackageToZenodo()] uploads an MR1 data package,
#' written to disk by [createDataPackage()], to [Zenodo](https://zenodo.org) as
#' a **draft** record: either a new record, or a new version of an existing
#' record. The function never publishes the record: check the draft on Zenodo
#' and publish it there.
#'
#' The package is first checked with [validateDataPackage()]; an invalid
#' package is not uploaded. The Zenodo metadata are taken from
#' `datapackage.json`:
#' * title, description, version, keywords and related identifiers;
#' * creators: all contributors, with ORCID and affiliation;
#' * contributors: each contributor with each of their roles, and the owner as
#'   rights holder;
#' * licence, and the embargo: if the embargo date is in the future, the record
#'   is `embargoed` (metadata public, files restricted until that date),
#'   otherwise `open`;
#' * location of the site, and the period of the data (`Collected`);
#' * the suggested citation, and the contact person(s) for data requests during
#'   the embargo, in the description and notes.
#'
#' A DOI is reserved for the record. It is written as `id` in the local
#' `datapackage.json` (and in the suggested citation) before the files are
#' uploaded, so the published package carries its own DOI.
#'
#' **New versions.** When the data of a published campaign are corrected,
#' create the package again with a higher `version`, and with the DOI of the
#' published record as `id` in the metadata file. The upload then creates a new
#' version of that record (with a new DOI) instead of a new record. All versions
#' share the concept DOI of the record. The files of the previous version are
#' replaced, and the related identifiers of the previous version that are not
#' in the package (e.g. `IsPartOf` links to collections, see
#' [addZenodoRelatedIdentifiers()]) are kept.
#' @param packageDir Directory of the data package, i.e. the folder with
#' `datapackage.json` written by [createDataPackage()].
#' @param token Zenodo personal access token with the scopes `deposit:write`
#' and `deposit:actions`. Default is the environment variable `ZENODO_TOKEN`
#' (or `ZENODO_SANDBOX_TOKEN` if `sandbox = TRUE`). Never write the token in a
#' script.
#' @param sandbox logical. If `TRUE`, upload to the Zenodo sandbox
#' (<https://sandbox.zenodo.org>) for testing, which needs its own account and
#' token. Default is `FALSE`.
#' @param community Identifier of the Zenodo community the record is submitted
#' to when it is published. Default is `"birdscanmr1community"`, the BirdScan
#' MR1 community. Use `NULL` for none, e.g. when testing in the sandbox, where
#' this community does not exist.
#' @param addDoi logical. If `TRUE` (default), the reserved DOI is written into
#' the local `datapackage.json` before the upload.
#' @param newVersion logical or `NULL`. If `TRUE`, create a new version of the
#' Zenodo record given by the `id` (DOI) of the package; if `FALSE`, create a
#' new record. Default is `NULL`: a new version if the package has a Zenodo DOI
#' as `id`, otherwise a new record.
#'
#' @return A list, invisibly, with `id` (Zenodo record ID of the draft), `doi`
#' (reserved DOI), `url` (link to the draft on Zenodo), `files` (names of the
#' uploaded files) and `newVersion` (logical).
#' @family write file functions
#' @export
#' @examples
#' \dontrun{
#' # Create the data package (see createDataPackage())
#' # ===========================================================================
#' dbData = readRDS(system.file("extdata",
#'   "CH_Sempach_2024_SEP24_25_DataExtract.rds",
#'   package = "birdscanR"
#' ))
#' mainOutputDir = file.path(".", "results")
#' metadataFile = file.path(mainOutputDir, "CH_Sempach_2024_metadata.yaml")
#' createDataPackageMetadataTemplate(filePath = metadataFile, overwrite = TRUE)
#' createDataPackage(
#'   dbExtract     = dbData,
#'   metadata      = metadataFile,
#'   outputDirPath = mainOutputDir
#' )
#'
#' # Upload the data package as draft to the Zenodo sandbox
#' # ===========================================================================
#' # The token is read from the environment variable ZENODO_SANDBOX_TOKEN, e.g.
#' # set in your .Renviron file
#' draft = uploadDataPackageToZenodo(
#'   packageDir = file.path(mainOutputDir, "CH_Sempach_2024"),
#'   sandbox    = TRUE,
#'   community  = NULL
#' )
#' draft$url
#' }
uploadDataPackageToZenodo = function(packageDir,
                                     token = NULL,
                                     sandbox = FALSE,
                                     community = "birdscanmr1community",
                                     addDoi = TRUE,
                                     newVersion = NULL) {
  # Input validation
  # ==============================================================================
  .checkHttr2()
  if (!is.character(packageDir) || length(packageDir) != 1) {
    stop("'packageDir' must be a single character string.")
  }
  packageJson = file.path(packageDir, "datapackage.json")
  if (!file.exists(packageJson)) {
    stop(paste0("No 'datapackage.json' found in 'packageDir': ", packageDir))
  }
  if (!is.logical(sandbox) || length(sandbox) != 1 || is.na(sandbox)) {
    stop("'sandbox' must be a single logical value (TRUE or FALSE).")
  }
  if (!is.null(community) && (!is.character(community) || length(community) != 1 || !nzchar(community))) {
    stop("'community' must be a single character string or NULL.")
  }
  if (!is.logical(addDoi) || length(addDoi) != 1 || is.na(addDoi)) {
    stop("'addDoi' must be a single logical value (TRUE or FALSE).")
  }
  if (!is.null(newVersion) && (!is.logical(newVersion) || length(newVersion) != 1 || is.na(newVersion))) {
    stop("'newVersion' must be TRUE, FALSE or NULL.")
  }
  token = .zenodoToken(token, sandbox)

  # Validate the data package
  # ==============================================================================
  validation = validateDataPackage(packageDir)
  if (!validation$valid) {
    print(validation)
    stop("The data package is not valid and was not uploaded. Fix the errors above first.")
  }
  package = jsonlite::read_json(packageJson, simplifyVector = FALSE)
  resourcePaths = unlist(lapply(package$resources, function(r) r$path))
  if (any(grepl("^https?://", resourcePaths))) {
    stop("All resources must be local files in 'packageDir'; remote paths cannot be uploaded.")
  }
  missingFiles = resourcePaths[!file.exists(file.path(packageDir, resourcePaths))]
  if (length(missingFiles) > 0) {
    stop(paste("The following resource files are missing:", paste(missingFiles, collapse = ", ")))
  }

  # New record or new version?
  # ==============================================================================
  hasZenodoId = .isZenodoId(package$id)
  if (is.null(newVersion)) newVersion = hasZenodoId
  if (newVersion && !hasZenodoId) {
    stop("'newVersion = TRUE' needs the DOI of the published Zenodo record as 'id' of the data package (set 'id' in the metadata file).")
  }
  if (!newVersion && !is.null(package$id)) {
    warning(paste0(
      "The data package already has an id (", package$id, "), but a new Zenodo record is created. ",
      "Use 'newVersion = TRUE' to create a new version of that record instead."
    ))
  }

  # Create the draft: a new record, or a new version of the latest version
  # ==============================================================================
  baseUrl = .zenodoBaseUrl(sandbox)
  if (newVersion) {
    previousId = .zenodoLatestRecordId(package$id, baseUrl, token)
    previous = .zenodoRequest(paste0(baseUrl, "/api/deposit/depositions/", previousId), token)
    if (!isTRUE(previous$submitted)) {
      stop(paste0("Zenodo record ", previousId, " is not published yet; a new version can only be made of a published record. Update the draft on Zenodo instead."))
    }
    if (identical(previous$metadata$version, package$version)) {
      warning(paste0("The new version has the same version number as the previous one (", package$version, "). Increase 'version' in the metadata file."))
    }
    response = .zenodoRequest(
      paste0(baseUrl, "/api/deposit/depositions/", previousId, "/actions/newversion"), token,
      method = "POST"
    )
    deposition = .zenodoRequest(response$links$latest_draft, token)
    previousRelated = previous$metadata$related_identifiers
  } else {
    deposition = .zenodoRequest(
      paste0(baseUrl, "/api/deposit/depositions"), token,
      method = "POST", json = list(metadata = list(prereserve_doi = TRUE))
    )
    previousRelated = NULL
  }
  depositionUrl = paste0(baseUrl, "/api/deposit/depositions/", deposition$id)
  draftUrl = deposition$links$html %||% paste0(baseUrl, "/deposit/", deposition$id)
  cleanupHint = paste0(
    " The draft record ", deposition$id, " was created; continue or delete it on ",
    draftUrl
  )

  files = c("datapackage.json", resourcePaths)
  doi = .zenodoReservedDoi(deposition$metadata$prereserve_doi$doi, sandbox)
  tryCatch(
    {
      if (newVersion) {
        # Remove the files copied from the previous version, and reserve a DOI
        # for the new version
        # ======================================================================
        for (f in deposition$files) {
          .zenodoRequest(paste0(depositionUrl, "/files/", f$id), token, method = "DELETE")
        }
        reserveMetadata = deposition$metadata
        reserveMetadata$doi = NULL
        reserveMetadata$prereserve_doi = TRUE
        deposition = .zenodoRequest(depositionUrl, token, method = "PUT", json = list(metadata = reserveMetadata))
        doi = .zenodoReservedDoi(deposition$metadata$prereserve_doi$doi, sandbox)
      }

      # Write the DOI into the package
      # ==========================================================================
      if (addDoi && !is.null(doi)) {
        package = .addDoiToPackage(package, doi)
        .writePackageJson(package, packageJson)
      }

      # Upload the files
      # ==========================================================================
      for (f in files) {
        .zenodoRequest(
          paste0(deposition$links$bucket, "/", utils::URLencode(basename(f))), token,
          method = "PUT", file = file.path(packageDir, f)
        )
      }

      # Add the metadata; keep the related identifiers of the previous version
      # ==========================================================================
      metadata = .zenodoMetadata(package, community = community)
      metadata$related_identifiers = .mergeZenodoRelatedIdentifiers(metadata$related_identifiers, previousRelated)
      if (length(metadata$related_identifiers) == 0) metadata$related_identifiers = NULL
      .zenodoRequest(depositionUrl, token, method = "PUT", json = list(metadata = metadata))
    },
    error = function(e) {
      stop(paste0(conditionMessage(e), cleanupHint), call. = FALSE)
    }
  )

  message(paste0(
    if (newVersion) "Draft of a new version created on Zenodo" else "Draft record created on Zenodo",
    if (sandbox) " (sandbox)", ": ", draftUrl,
    if (!is.null(doi)) paste0("\nReserved DOI: ", doi),
    "\nCheck the draft and publish it on Zenodo.",
    if (!newVersion) " Then add the DOI as 'id' to the metadata file of the campaign."
  ))
  invisible(list(
    id         = deposition$id,
    doi        = doi,
    url        = draftUrl,
    files      = basename(files),
    newVersion = newVersion
  ))
}

#' Zenodo licence identifiers of the allowed MR1 data package licences
#' @noRd
.zenodoLicenses = c("CC-BY-4.0" = "cc-by-4.0", "CC-BY-NC-4.0" = "cc-by-nc-4.0")

#' Add a DOI to a data package
#' @description Sets `id` to the DOI URL. In the suggested citation, the
#' previous `id` (e.g. the DOI of the previous version) is replaced by the new
#' DOI, or the DOI is added if it is not in there yet.
#' @param package data package (list read from datapackage.json)
#' @param doi DOI, e.g. "10.5281/zenodo.123"
#' @return data package
#' @noRd
.addDoiToPackage = function(package, doi) {
  doiUrl = paste0("https://doi.org/", doi)
  previousId = package$id
  package$id = doiUrl
  citation = package$bibliographicCitation
  if (!is.null(citation) && !grepl(doi, citation, fixed = TRUE)) {
    if (!is.null(previousId) && nzchar(previousId) && grepl(previousId, citation, fixed = TRUE)) {
      package$bibliographicCitation = sub(previousId, doiUrl, citation, fixed = TRUE)
    } else {
      package$bibliographicCitation = paste(citation, doiUrl)
    }
  }
  # Keep id after name, as written by createDataPackage()
  others = setdiff(names(package), c("name", "id"))
  package[c(intersect("name", names(package)), "id", others)]
}

#' Write a data package (list) as datapackage.json
#' @noRd
.writePackageJson = function(package, filePath) {
  packageJson = jsonlite::toJSON(package,
    pretty = TRUE, null = "null", na = "null", auto_unbox = TRUE, digits = NA
  )
  write(packageJson, filePath)
}

#' Escape text for HTML
#' @noRd
.htmlEscape = function(x) {
  x = gsub("&", "&amp;", x, fixed = TRUE)
  x = gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

#' Zenodo creators and contributors from the contributors of the metadata
#' @param contributors list of contributors (title, givenName, familyName,
#' path (ORCID), organization, roles)
#' @param ownerName name of the owner, added as rights holder; or NULL
#' @return list with `creators` and `contributors` (one entry per role)
#' @noRd
.zenodoPeople = function(contributors, ownerName = NULL) {
  person = function(ctb) {
    name = if (!is.null(ctb$familyName) && !is.null(ctb$givenName)) {
      paste0(ctb$familyName, ", ", ctb$givenName)
    } else {
      ctb$title
    }
    out = list(name = name, affiliation = ctb$organization)
    if (!is.null(ctb$path)) out$orcid = sub("^https?://orcid.org/", "", ctb$path)
    out[!vapply(out, is.null, logical(1))]
  }
  creators = lapply(contributors, person)
  zenodoContributors = list()
  for (ctb in contributors) {
    for (role in unlist(ctb$roles)) {
      zenodoContributors[[length(zenodoContributors) + 1]] = c(person(ctb), type = role)
    }
  }
  if (!is.null(ownerName)) {
    zenodoContributors[[length(zenodoContributors) + 1]] = list(name = ownerName, type = "RightsHolder")
  }
  list(creators = creators, contributors = zenodoContributors)
}

#' Build the Zenodo deposit metadata of an MR1 data package
#' @param package data package (list read from datapackage.json)
#' @param community Zenodo community identifier, or NULL
#' @param today date used to decide whether the embargo has ended
#' @return named list with the Zenodo deposit metadata
#' @noRd
.zenodoMetadata = function(package, community = NULL, today = Sys.Date()) {
  contributors = package$contributors

  # Creators and contributors (one entry per role)
  # ============================================================================
  people = .zenodoPeople(contributors, package$owner$name)

  # Access: embargoed until the embargo date, open afterwards
  # ============================================================================
  embargo = as.Date(package$embargo)
  embargoed = !is.na(embargo) && embargo > today

  # Description with site, period and suggested citation (HTML)
  # ============================================================================
  site = package$site
  descriptionParts = c(
    paste0("<p>", .htmlEscape(package$description), "</p>"),
    if (!is.null(site$siteName)) {
      paste0(
        "<p><strong>Site:</strong> ", .htmlEscape(site$siteName),
        if (!is.null(site$siteCode)) paste0(" (", .htmlEscape(site$siteCode), ")"),
        ", latitude ", site$latitude, ", longitude ", site$longitude,
        if (!is.null(site$altitude)) paste0(", altitude ", site$altitude, " m"), ".</p>"
      )
    },
    if (!is.null(package$temporal)) {
      paste0("<p><strong>Period:</strong> ", package$temporal$start, " to ", package$temporal$end, ".</p>")
    },
    "<p>The data package follows the Frictionless Data Package standard (https://datapackage.org): <code>datapackage.json</code> describes the tables and their columns.</p>",
    if (!is.null(package$bibliographicCitation)) {
      paste0("<p><strong>Suggested citation:</strong> ", .htmlEscape(package$bibliographicCitation), "</p>")
    }
  )

  # Notes: who to contact for data requests during the embargo
  # ============================================================================
  contacts = Filter(function(ctb) "ContactPerson" %in% unlist(ctb$roles), contributors)
  notes = NULL
  if (embargoed && length(contacts) > 0) {
    notes = paste0(
      "The data files are under embargo until ", format(embargo, "%Y-%m-%d"),
      ". During the embargo, request the data from: ",
      paste(vapply(contacts, function(ctb) {
        paste0(ctb$title, if (!is.null(ctb$email)) paste0(" (", ctb$email, ")"))
      }, character(1)), collapse = ", "), "."
    )
  }

  # Related identifiers: DataCite relation types in Zenodo notation
  # ============================================================================
  relatedIdentifiers = .zenodoRelatedIdentifiers(package$relatedIdentifiers)

  # Assemble
  # ============================================================================
  licenseName = package$licenses[[1]]$name
  md = list(
    upload_type = "dataset",
    title = package$title,
    description = paste(descriptionParts, collapse = "\n"),
    version = package$version,
    creators = people$creators,
    contributors = people$contributors,
    access_right = if (embargoed) "embargoed" else "open",
    embargo_date = if (embargoed) format(embargo, "%Y-%m-%d"),
    license = unname(.zenodoLicenses[licenseName]) %||% tolower(licenseName),
    keywords = if (length(package$keywords) > 0) I(unlist(package$keywords)),
    related_identifiers = if (length(relatedIdentifiers) > 0) relatedIdentifiers,
    notes = notes,
    locations = if (!is.null(site$latitude) && !is.null(site$longitude)) {
      list(list(lat = site$latitude, lon = site$longitude, place = site$siteName %||% package$campaignID))
    },
    dates = if (!is.null(package$temporal)) {
      list(list(start = package$temporal$start, end = package$temporal$end, type = "Collected"))
    },
    communities = if (!is.null(community)) list(list(identifier = community)),
    # Keep the reserved DOI; Zenodo registers it when the record is published
    prereserve_doi = TRUE
  )
  md[!vapply(md, is.null, logical(1))]
}
