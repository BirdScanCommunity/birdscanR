#' @title Upload MR1 Data Package to Zenodo
#' @author Birgen Haest
#' @description [uploadDataPackageToZenodo()] uploads an MR1 data package,
#' written to disk by [createDataPackage()], to [Zenodo](https://zenodo.org) as
#' a **draft** record. The function never publishes the record: check the draft
#' on Zenodo and publish it there.
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
#' `datapackage.json` (and added to the suggested citation) before the files
#' are uploaded, so the published package carries its own DOI. Also add it as
#' `id` to the metadata file of the campaign, so new versions of the package
#' keep it.
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
#'
#' @return A list, invisibly, with `id` (Zenodo record ID), `doi` (reserved
#' DOI), `url` (link to the draft on Zenodo) and `files` (names of the uploaded
#' files).
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
                                     addDoi = TRUE) {
  # Input validation
  # ==============================================================================
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("The package 'httr2' is required to upload to Zenodo. Install it with install.packages(\"httr2\").")
  }
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
  if (is.null(token)) {
    token = Sys.getenv(if (sandbox) "ZENODO_SANDBOX_TOKEN" else "ZENODO_TOKEN")
  }
  if (!is.character(token) || length(token) != 1 || !nzchar(token)) {
    stop(paste0(
      "No Zenodo token found. Create a personal access token on ",
      if (sandbox) "sandbox.zenodo.org" else "zenodo.org",
      " and store it in the environment variable ",
      if (sandbox) "ZENODO_SANDBOX_TOKEN" else "ZENODO_TOKEN",
      " (e.g. in your .Renviron file)."
    ))
  }

  # Validate the data package
  # ==============================================================================
  validation = validateDataPackage(packageDir)
  if (!validation$valid) {
    print(validation)
    stop("The data package is not valid and was not uploaded. Fix the errors above first.")
  }
  package = jsonlite::read_json(packageJson, simplifyVector = FALSE)
  if (!is.null(package$id)) {
    warning(paste0(
      "The data package already has an id (", package$id, "). A new Zenodo record ",
      "is created; new versions of existing records are not supported yet."
    ))
  }
  resourcePaths = unlist(lapply(package$resources, function(r) r$path))
  if (any(grepl("^https?://", resourcePaths))) {
    stop("All resources must be local files in 'packageDir'; remote paths cannot be uploaded.")
  }
  missingFiles = resourcePaths[!file.exists(file.path(packageDir, resourcePaths))]
  if (length(missingFiles) > 0) {
    stop(paste("The following resource files are missing:", paste(missingFiles, collapse = ", ")))
  }

  # Create the draft record, with a reserved DOI
  # ==============================================================================
  baseUrl = if (sandbox) "https://sandbox.zenodo.org" else "https://zenodo.org"
  deposition = .zenodoRequest(
    paste0(baseUrl, "/api/deposit/depositions"), token,
    method = "POST", json = list(metadata = list(prereserve_doi = TRUE))
  )
  doi = deposition$metadata$prereserve_doi$doi
  draftUrl = deposition$links$html %||%
    paste0(baseUrl, "/deposit/", deposition$id)
  cleanupHint = paste0(
    " The draft record ", deposition$id, " was created; continue or delete it on ",
    draftUrl
  )

  tryCatch(
    {
      # Write the DOI into the package
      # ==========================================================================
      if (addDoi && !is.null(doi)) {
        package = .addDoiToPackage(package, doi)
        .writePackageJson(package, packageJson)
      }

      # Upload the files
      # ==========================================================================
      files = c("datapackage.json", resourcePaths)
      for (f in files) {
        .zenodoRequest(
          paste0(deposition$links$bucket, "/", utils::URLencode(basename(f))), token,
          method = "PUT", file = file.path(packageDir, f)
        )
      }

      # Add the metadata
      # ==========================================================================
      .zenodoRequest(
        paste0(baseUrl, "/api/deposit/depositions/", deposition$id), token,
        method = "PUT",
        json = list(metadata = .zenodoMetadata(package, community = community))
      )
    },
    error = function(e) {
      stop(paste0(conditionMessage(e), cleanupHint), call. = FALSE)
    }
  )

  message(paste0(
    "Draft record created on Zenodo", if (sandbox) " (sandbox)", ": ", draftUrl,
    if (!is.null(doi)) paste0("\nReserved DOI: ", doi),
    "\nCheck the draft and publish it on Zenodo. Add the DOI as 'id' to the metadata file of the campaign."
  ))
  invisible(list(
    id    = deposition$id,
    doi   = doi,
    url   = draftUrl,
    files = basename(files)
  ))
}

#' Zenodo licence identifiers of the allowed MR1 data package licences
#' @noRd
.zenodoLicenses = c("CC-BY-4.0" = "cc-by-4.0", "CC-BY-NC-4.0" = "cc-by-nc-4.0")

#' Send a request to the Zenodo REST API
#' @param url request URL
#' @param token Zenodo personal access token
#' @param method HTTP method
#' @param json list sent as JSON body, or NULL
#' @param file path of a file sent as body, or NULL
#' @return parsed JSON response (list)
#' @noRd
.zenodoRequest = function(url, token, method = "GET", json = NULL, file = NULL) {
  req = httr2::request(url)
  req = httr2::req_auth_bearer_token(req, token)
  req = httr2::req_method(req, method)
  req = httr2::req_user_agent(req, paste0("birdscanR/", utils::packageVersion("birdscanR")))
  if (!is.null(json)) req = httr2::req_body_json(req, json, auto_unbox = TRUE, null = "null", digits = NA)
  if (!is.null(file)) req = httr2::req_body_file(req, file, type = "application/octet-stream")
  req = httr2::req_error(req, body = function(resp) .zenodoErrorMessage(resp))
  resp = httr2::req_perform(req)
  if (length(httr2::resp_body_raw(resp)) == 0) {
    return(list())
  }
  httr2::resp_body_json(resp)
}

#' Extract the error message(s) from a Zenodo API response
#' @noRd
.zenodoErrorMessage = function(resp) {
  body = tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (is.null(body)) {
    return(NULL)
  }
  details = vapply(body$errors %||% list(), function(e) {
    paste0(e$field %||% "", if (!is.null(e$field)) ": ", paste(unlist(e$message %||% e$messages), collapse = " "))
  }, character(1))
  c(body$message, details)
}

#' Add a DOI to a data package
#' @description Sets `id` to the DOI URL and adds the DOI to the suggested
#' citation if it is not in there yet.
#' @param package data package (list read from datapackage.json)
#' @param doi DOI, e.g. "10.5281/zenodo.123"
#' @return data package
#' @noRd
.addDoiToPackage = function(package, doi) {
  doiUrl = paste0("https://doi.org/", doi)
  package$id = doiUrl
  if (!is.null(package$bibliographicCitation) && !grepl(doi, package$bibliographicCitation, fixed = TRUE)) {
    package$bibliographicCitation = paste(package$bibliographicCitation, doiUrl)
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

#' Build the Zenodo deposit metadata of an MR1 data package
#' @param package data package (list read from datapackage.json)
#' @param community Zenodo community identifier, or NULL
#' @param today date used to decide whether the embargo has ended
#' @return named list with the Zenodo deposit metadata
#' @noRd
.zenodoMetadata = function(package, community = NULL, today = Sys.Date()) {
  contributors = package$contributors
  personName = function(ctb) {
    if (!is.null(ctb$familyName) && !is.null(ctb$givenName)) {
      paste0(ctb$familyName, ", ", ctb$givenName)
    } else {
      ctb$title
    }
  }
  person = function(ctb) {
    out = list(name = personName(ctb), affiliation = ctb$organization)
    if (!is.null(ctb$path)) out$orcid = sub("^https?://orcid.org/", "", ctb$path)
    out[!vapply(out, is.null, logical(1))]
  }

  # Creators and contributors (one entry per role)
  # ============================================================================
  creators = lapply(contributors, person)
  zenodoContributors = list()
  for (ctb in contributors) {
    for (role in unlist(ctb$roles)) {
      zenodoContributors[[length(zenodoContributors) + 1]] = c(person(ctb), type = role)
    }
  }
  if (!is.null(package$owner$name)) {
    zenodoContributors[[length(zenodoContributors) + 1]] = list(name = package$owner$name, type = "RightsHolder")
  }

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
  relatedIdentifiers = lapply(package$relatedIdentifiers, function(ri) {
    relation = ri$relationType
    list(
      identifier = ri$relatedIdentifier,
      relation = paste0(tolower(substr(relation, 1, 1)), substring(relation, 2))
    )
  })

  # Assemble
  # ============================================================================
  licenseName = package$licenses[[1]]$name
  md = list(
    upload_type = "dataset",
    title = package$title,
    description = paste(descriptionParts, collapse = "\n"),
    version = package$version,
    creators = creators,
    contributors = zenodoContributors,
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
