#' @title Create MR1 Collection Metadata Template
#' @author Birgen Haest
#' @description [createCollectionMetadataTemplate()] writes a commented YAML
#' template with the metadata of a collection of MR1 records on Zenodo:
#' collection ID, level (`owner` or `community`), title, description, version,
#' owner, licence, keywords, contributors, members (DOIs of the records in the
#' collection), related identifiers and suggested citation. Fill in the
#' template and pass its path to the `metadata` argument of
#' [uploadCollectionToZenodo()].
#' @param filePath Path of the YAML file to write.
#' @param overwrite logical. If `TRUE`, an existing file is overwritten. Default
#' is `FALSE`.
#'
#' @return The path of the written file, invisibly.
#' @family write file functions
#' @export
#' @examples
#' # Write the collection metadata template
#' # ===========================================================================
#' metadataFile = file.path(tempdir(), "SOI_BirdScanMR1_collection.yaml")
#' createCollectionMetadataTemplate(filePath = metadataFile, overwrite = TRUE)
createCollectionMetadataTemplate = function(filePath, overwrite = FALSE) {
  if (!is.character(filePath) || length(filePath) != 1) {
    stop("'filePath' must be a single character string.")
  }
  if (file.exists(filePath) && !isTRUE(overwrite)) {
    stop(paste0("'filePath' already exists: ", filePath, ". Use 'overwrite = TRUE' to replace it."))
  }
  templatePath = system.file("extdata", "dataPackage", "collectionMetadataTemplate.yaml",
    package = "birdscanR"
  )
  file.copy(templatePath, filePath, overwrite = TRUE)
  invisible(filePath)
}

#' @title Upload a Collection of MR1 Records to Zenodo
#' @author Birgen Haest
#' @description [uploadCollectionToZenodo()] creates a **collection record** on
#' [Zenodo](https://zenodo.org) as a **draft**: a record with its own DOI that
#' groups published Zenodo records, so that they can be cited together. There
#' are two levels of collections:
#' * an **owner collection** groups the MR1 data packages (campaigns) of one
#'   owner, e.g. all campaigns of the Swiss Ornithological Institute;
#' * the **community collection** groups the owner collections of the BirdScan
#'   MR1 community.
#'
#' The collection is described in a YAML file, see
#' [createCollectionMetadataTemplate()]. The collection record:
#' * links to each member with a `HasPart` related identifier. The links point
#'   to the concept DOI of the member, so they stay valid when a member gets a
#'   new version;
#' * holds the file `members.csv`, with per member the concept DOI, the DOI,
#'   record ID, title, version and publication date of its latest version, its
#'   period and its Zenodo link, at the time of the upload;
#' * gets the period (`Collected`) of its members.
#'
#' With `linkMembers = TRUE` (default), an `IsPartOf` link to the concept DOI
#' of the collection is added to each member, with
#' [addZenodoRelatedIdentifiers()]. These edits of the members are **not
#' published**, because the collection itself is a draft: first check and
#' publish the collection on Zenodo, then publish the edits of the members with
#' [linkCollectionMembers()] and `publish = TRUE` (or on Zenodo). Links can
#' only be added to records of your own Zenodo account; members that cannot be
#' linked are reported in a warning.
#'
#' **New versions.** To add or remove members, add them to the `members` of the
#' metadata file, increase `version`, and set `id` to the DOI of the published
#' collection. The upload then creates a new version of the collection record.
#' The `HasPart` links follow the new list of members; other related
#' identifiers of the previous version are kept.
#' @param metadata Path to the YAML file with the collection metadata (see
#' [createCollectionMetadataTemplate()]), or a named list with the same
#' structure.
#' @param token Zenodo personal access token with the scopes `deposit:write`
#' and `deposit:actions`. Default is the environment variable `ZENODO_TOKEN`
#' (or `ZENODO_SANDBOX_TOKEN` if `sandbox = TRUE`). Never write the token in a
#' script.
#' @param sandbox logical. If `TRUE`, use the Zenodo sandbox
#' (<https://sandbox.zenodo.org>). Default is `FALSE`.
#' @param community Identifier of the Zenodo community the record is submitted
#' to when it is published. Default is `"birdscanmr1community"`, the BirdScan
#' MR1 community. Use `NULL` for none, e.g. when testing in the sandbox.
#' @param newVersion logical or `NULL`. If `TRUE`, create a new version of the
#' Zenodo record given by `id` in the metadata; if `FALSE`, create a new
#' record. Default is `NULL`: a new version if the metadata have a Zenodo DOI
#' as `id`, otherwise a new record.
#' @param linkMembers logical. If `TRUE` (default), add an `IsPartOf` link to
#' the collection to each member (not published, see Description).
#'
#' @return A list, invisibly, with `id` (Zenodo record ID of the draft), `doi`
#' (reserved DOI), `conceptDoi` (concept DOI of the collection), `url` (link to
#' the draft on Zenodo), `members` (data frame, the content of `members.csv`),
#' `links` (data frame with the result of linking each member, or `NULL`) and
#' `newVersion` (logical).
#' @family write file functions
#' @export
#' @examples
#' \dontrun{
#' # Describe the collection: fill in the template, with the DOIs of the
#' # published campaign records as members
#' # ===========================================================================
#' metadataFile = file.path(".", "SOI_BirdScanMR1_collection.yaml")
#' createCollectionMetadataTemplate(filePath = metadataFile)
#'
#' # Upload the collection as draft to the Zenodo sandbox
#' # ===========================================================================
#' draft = uploadCollectionToZenodo(
#'   metadata  = metadataFile,
#'   sandbox   = TRUE,
#'   community = NULL
#' )
#' draft$members
#'
#' # After publishing the collection on Zenodo: publish the IsPartOf links of
#' # the members
#' # ===========================================================================
#' linkCollectionMembers(doi = draft$doi, sandbox = TRUE, publish = TRUE)
#' }
uploadCollectionToZenodo = function(metadata,
                                    token = NULL,
                                    sandbox = FALSE,
                                    community = "birdscanmr1community",
                                    newVersion = NULL,
                                    linkMembers = TRUE) {
  # Input validation
  # ==============================================================================
  .checkHttr2()
  md = .readPackageMetadata(metadata)
  problems = .checkCollectionMetadata(md)
  if (length(problems) > 0) {
    stop(paste0(
      "The collection metadata are not valid:\n",
      paste0("- ", problems, collapse = "\n")
    ), call. = FALSE)
  }
  if (!is.logical(sandbox) || length(sandbox) != 1 || is.na(sandbox)) {
    stop("'sandbox' must be a single logical value (TRUE or FALSE).")
  }
  if (!is.null(community) && (!is.character(community) || length(community) != 1 || !nzchar(community))) {
    stop("'community' must be a single character string or NULL.")
  }
  if (!is.null(newVersion) && (!is.logical(newVersion) || length(newVersion) != 1 || is.na(newVersion))) {
    stop("'newVersion' must be TRUE, FALSE or NULL.")
  }
  if (!is.logical(linkMembers) || length(linkMembers) != 1 || is.na(linkMembers)) {
    stop("'linkMembers' must be a single logical value (TRUE or FALSE).")
  }
  hasZenodoId = .isZenodoId(md$id)
  if (is.null(newVersion)) newVersion = hasZenodoId
  if (newVersion && !hasZenodoId) {
    stop("'newVersion = TRUE' needs the DOI of the published Zenodo record as 'id' in the collection metadata.")
  }
  if (!newVersion && !is.null(md$id)) {
    warning(paste0(
      "The collection already has an id (", md$id, "), but a new Zenodo record is created. ",
      "Use 'newVersion = TRUE' to create a new version of that record instead."
    ))
  }
  token = .zenodoToken(token, sandbox)
  baseUrl = .zenodoBaseUrl(sandbox)

  # Get the latest published version of each member
  # ==============================================================================
  memberRecords = lapply(unlist(md$members), function(member) {
    tryCatch(
      .zenodoRequest(paste0(baseUrl, "/api/records/", .zenodoRecordId(member), "/versions/latest"), token),
      error = function(e) {
        stop(paste0(
          "Member ", member, " is not a published Zenodo record",
          if (sandbox) " in the sandbox", ": ", conditionMessage(e)
        ), call. = FALSE)
      }
    )
  })
  members = .collectionMembers(memberRecords)
  dupes = unique(members$conceptDoi[duplicated(members$conceptDoi)])
  if (length(dupes) > 0) {
    stop(paste0(
      "The following records are listed more than once as member (possibly as different versions): ",
      paste(dupes, collapse = ", ")
    ))
  }
  membersFile = file.path(tempfile("collection"), "members.csv")
  dir.create(dirname(membersFile))
  on.exit(unlink(dirname(membersFile), recursive = TRUE), add = TRUE)
  utils::write.csv(members, membersFile, row.names = FALSE, na = "", fileEncoding = "UTF-8")

  # Create the draft: a new record, or a new version of the latest version
  # ==============================================================================
  if (newVersion) {
    previousId = .zenodoLatestRecordId(md$id, baseUrl, token)
    previous = .zenodoRequest(paste0(baseUrl, "/api/deposit/depositions/", previousId), token)
    if (!isTRUE(previous$submitted)) {
      stop(paste0("Zenodo record ", previousId, " is not published yet; a new version can only be made of a published record. Update the draft on Zenodo instead."))
    }
    if (identical(previous$metadata$version, md$version)) {
      warning(paste0("The new version has the same version number as the previous one (", md$version, "). Increase 'version' in the metadata file."))
    }
    response = .zenodoRequest(
      paste0(baseUrl, "/api/deposit/depositions/", previousId, "/actions/newversion"), token,
      method = "POST"
    )
    deposition = .zenodoRequest(response$links$latest_draft, token)
    # The members define the HasPart links; keep the other links
    previousRelated = Filter(
      function(ri) !identical(tolower(ri$relation %||% ""), "haspart"),
      previous$metadata$related_identifiers %||% list()
    )
    conceptDoi = .zenodoConceptDoi(previous, sandbox = sandbox)
  } else {
    deposition = .zenodoRequest(
      paste0(baseUrl, "/api/deposit/depositions"), token,
      method = "POST", json = list(metadata = list(prereserve_doi = TRUE))
    )
    previousRelated = NULL
    conceptDoi = NULL
  }
  depositionUrl = paste0(baseUrl, "/api/deposit/depositions/", deposition$id)
  draftUrl = deposition$links$html %||% paste0(baseUrl, "/deposit/", deposition$id)
  cleanupHint = paste0(
    " The draft record ", deposition$id, " was created; continue or delete it on ",
    draftUrl
  )

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
      conceptDoi = conceptDoi %||% .zenodoConceptDoi(deposition, doi, sandbox = sandbox)

      # Upload the member list
      # ==========================================================================
      .zenodoRequest(
        paste0(deposition$links$bucket, "/members.csv"), token,
        method = "PUT", file = membersFile
      )

      # Add the metadata
      # ==========================================================================
      zenodoMetadata = .zenodoCollectionMetadata(md, members, doi = doi, community = community)
      zenodoMetadata$related_identifiers = .mergeZenodoRelatedIdentifiers(zenodoMetadata$related_identifiers, previousRelated)
      .zenodoRequest(depositionUrl, token, method = "PUT", json = list(metadata = zenodoMetadata))
    },
    error = function(e) {
      stop(paste0(conditionMessage(e), cleanupHint), call. = FALSE)
    }
  )

  # Link the members to the collection (edits are not published)
  # ==============================================================================
  links = NULL
  if (linkMembers) {
    if (is.null(conceptDoi)) {
      warning("The concept DOI of the collection is unknown; the members were not linked. Use linkCollectionMembers() after publishing the collection.")
    } else {
      links = .linkMembers(members$recordId, conceptDoi, token = token, sandbox = sandbox, publish = FALSE)
    }
  }

  message(paste0(
    if (newVersion) "Draft of a new version of the collection created on Zenodo" else "Draft collection record created on Zenodo",
    if (sandbox) " (sandbox)", ": ", draftUrl,
    if (!is.null(doi)) paste0("\nReserved DOI: ", doi),
    if (!is.null(conceptDoi)) paste0("\nConcept DOI: ", conceptDoi),
    "\nMembers: ", nrow(members),
    if (!is.null(links)) {
      paste0(
        "; IsPartOf links added to ", sum(links$added > 0), ", already present in ",
        sum(links$added == 0 & is.na(links$error)), ", failed for ", sum(!is.na(links$error))
      )
    },
    ".\nCheck the draft and publish it on Zenodo.",
    if (!is.null(links) && any(links$added > 0)) {
      " Then publish the IsPartOf links of the members with linkCollectionMembers(doi, publish = TRUE)."
    },
    if (!newVersion) " Add the DOI as 'id' to the collection metadata file."
  ))
  invisible(list(
    id         = deposition$id,
    doi        = doi,
    conceptDoi = conceptDoi,
    url        = draftUrl,
    members    = members,
    links      = links,
    newVersion = newVersion
  ))
}

#' @title Link the Members of a Zenodo Collection to the Collection
#' @author Birgen Haest
#' @description [linkCollectionMembers()] adds an `IsPartOf` link to the
#' concept DOI of a collection record (see [uploadCollectionToZenodo()]) to each
#' member of the collection, i.e. to each Zenodo record the collection links to
#' with `HasPart`. Members that already have the link are left unchanged.
#'
#' Use it after publishing a collection, with `publish = TRUE`, to publish the
#' links that [uploadCollectionToZenodo()] added to the members, or to link the
#' members of an existing collection. Links can only be added to records of
#' your own Zenodo account; members that cannot be linked are reported in a
#' warning.
#' @param doi DOI (e.g. `"https://doi.org/10.5281/zenodo.1234567"` or
#' `"10.5281/zenodo.1234567"`) or record ID of the collection record. Any
#' version, or the concept DOI, can be given; the members of the latest version
#' are linked.
#' @inheritParams addZenodoRelatedIdentifiers
#' @param publish logical. If `TRUE`, the edits of the members are published
#' directly, including edits with the link that are already open. Default is
#' `FALSE`: the edits stay open on Zenodo.
#'
#' @return A data frame, invisibly, with per member: `member` (Zenodo record
#' ID), `added` (number of links added), `published` (logical) and `error`
#' (error message, or `NA`).
#' @family write file functions
#' @export
#' @examples
#' \dontrun{
#' # Publish the IsPartOf links of the members of a published collection
#' # ===========================================================================
#' linkCollectionMembers(
#'   doi     = "https://doi.org/10.5281/zenodo.7654321",
#'   publish = TRUE
#' )
#' }
linkCollectionMembers = function(doi,
                                 token = NULL,
                                 sandbox = FALSE,
                                 publish = FALSE) {
  # Input validation
  # ==============================================================================
  .checkHttr2()
  recordId = .zenodoRecordId(doi)
  if (!is.logical(sandbox) || length(sandbox) != 1 || is.na(sandbox)) {
    stop("'sandbox' must be a single logical value (TRUE or FALSE).")
  }
  if (!is.logical(publish) || length(publish) != 1 || is.na(publish)) {
    stop("'publish' must be a single logical value (TRUE or FALSE).")
  }
  token = .zenodoToken(token, sandbox)
  baseUrl = .zenodoBaseUrl(sandbox)

  # Get the collection and its members
  # ==============================================================================
  recordId = .zenodoLatestRecordId(recordId, baseUrl, token)
  collection = .zenodoRequest(paste0(baseUrl, "/api/deposit/depositions/", recordId), token)
  conceptDoi = .zenodoConceptDoi(collection, sandbox = sandbox)
  if (is.null(conceptDoi)) {
    stop(paste0("The concept DOI of Zenodo record ", recordId, " is unknown."))
  }
  if (!isTRUE(collection$submitted)) {
    warning(paste0(
      "Zenodo record ", recordId, " is not published yet: the members are linked to its reserved concept DOI (",
      conceptDoi, "), which only resolves once the collection is published."
    ))
  }
  hasPart = Filter(
    function(ri) identical(tolower(ri$relation %||% ""), "haspart"),
    collection$metadata$related_identifiers %||% list()
  )
  memberIds = unlist(lapply(hasPart, function(ri) ri$identifier))
  memberIds = memberIds[vapply(memberIds, .isZenodoId, logical(1))]
  if (length(memberIds) == 0) {
    stop(paste0("Zenodo record ", recordId, " has no HasPart links to Zenodo records."))
  }

  # Link the members
  # ==============================================================================
  links = .linkMembers(memberIds, conceptDoi, token = token, sandbox = sandbox, publish = publish)
  message(paste0(
    "Members of collection ", conceptDoi, ": ", nrow(links),
    "; IsPartOf links added to ", sum(links$added > 0),
    ", published for ", sum(links$published),
    ", failed for ", sum(!is.na(links$error)), "."
  ))
  invisible(links)
}

#' Check MR1 collection metadata
#' @param md named list with collection metadata
#' @return character vector with problems; empty if none
#' @noRd
.checkCollectionMetadata = function(md) {
  problems = character(0)
  isString = function(x) is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
  for (field in c("collectionID", "level", "title", "description", "version")) {
    if (!isString(md[[field]])) {
      problems = c(problems, paste0("'", field, "' is required and must be a single, non-empty string."))
    }
  }
  if (isString(md$collectionID) && !grepl("^[A-Za-z0-9._-]+$", md$collectionID)) {
    problems = c(problems, "'collectionID' may only contain letters, digits, '.', '-' and '_'.")
  }
  if (isString(md$level) && !md$level %in% c("owner", "community")) {
    problems = c(problems, "'level' must be 'owner' or 'community'.")
  }
  if (!is.list(md$owner) || !isString(md$owner$name)) {
    problems = c(problems, "'owner' is required and must have a 'name'.")
  }
  if (!is.null(md$id) && !.isZenodoId(md$id)) {
    problems = c(problems, "'id' must be the DOI of a Zenodo record, or empty.")
  }
  members = unlist(md$members)
  if (length(members) == 0) {
    problems = c(problems, "'members' is required and must contain at least one DOI of a Zenodo record.")
  } else {
    notZenodo = members[!vapply(members, .isZenodoId, logical(1))]
    if (length(notZenodo) > 0) {
      problems = c(problems, paste0(
        "'members' must be DOIs or record IDs of Zenodo records. Not valid: ",
        paste(notZenodo, collapse = ", ")
      ))
    }
  }
  problems = c(problems, .checkSharedMetadata(md, embargoRequestType = FALSE))
  return(unique(problems))
}

#' Table of the members of a collection
#' @param records list of Zenodo records (latest version of each member), as
#' returned by the Zenodo records API
#' @return data frame with one row per member
#' @noRd
.collectionMembers = function(records) {
  doiUrl = function(x) if (is.null(x) || !nzchar(x)) NA_character_ else paste0("https://doi.org/", sub("^https?://(dx\\.)?doi\\.org/", "", x))
  value = function(x) if (is.null(x)) NA_character_ else as.character(x)
  rows = lapply(records, function(rec) {
    # The records API writes date types in lower case ("collected")
    collected = Filter(function(d) identical(tolower(d$type %||% ""), "collected"), rec$metadata$dates %||% list())
    starts = unlist(lapply(collected, function(d) d$start))
    ends = unlist(lapply(collected, function(d) d$end))
    data.frame(
      conceptDoi      = doiUrl(rec$conceptdoi %||% rec$doi),
      doi             = doiUrl(rec$doi),
      recordId        = as.integer(rec$id),
      title           = value(rec$metadata$title),
      version         = value(rec$metadata$version),
      publicationDate = value(rec$metadata$publication_date),
      start           = if (length(starts) > 0) min(starts) else NA_character_,
      end             = if (length(ends) > 0) max(ends) else NA_character_,
      url             = value(rec$links$self_html %||% rec$links$html),
      stringsAsFactors = FALSE
    )
  })
  out = do.call(rbind, rows)
  rownames(out) = NULL
  out
}

#' Concept DOI of a Zenodo deposition
#' @description Taken from `conceptdoi` (published records), or derived from
#' the concept record ID and the prefix of the (reserved) DOI (drafts).
#' @param deposition Zenodo deposition (list)
#' @param doi DOI of the deposition, if known
#' @param sandbox logical. Is the deposition on the Zenodo sandbox?
#' @return concept DOI, e.g. "10.5281/zenodo.100", or NULL
#' @noRd
.zenodoConceptDoi = function(deposition, doi = NULL, sandbox = FALSE) {
  isSet = function(x) is.character(x) && length(x) == 1 && nzchar(x)
  if (isSet(deposition$conceptdoi)) {
    return(deposition$conceptdoi)
  }
  candidates = Filter(isSet, list(
    doi, deposition$doi, deposition$metadata$doi,
    .zenodoReservedDoi(deposition$metadata$prereserve_doi$doi, sandbox)
  ))
  if (is.null(deposition$conceptrecid) || length(candidates) == 0 || !grepl("zenodo\\.[0-9]+$", candidates[[1]])) {
    return(NULL)
  }
  sub("[0-9]+$", as.character(deposition$conceptrecid), candidates[[1]])
}

#' Add IsPartOf links to a collection to its members
#' @param memberIds DOIs or record IDs of the members
#' @param conceptDoi concept DOI of the collection
#' @return data frame with member, added, published and error per member
#' @noRd
.linkMembers = function(memberIds, conceptDoi, token, sandbox, publish) {
  isPartOf = list(list(
    relationType = "IsPartOf",
    relatedIdentifier = paste0("https://doi.org/", conceptDoi)
  ))
  rows = lapply(memberIds, function(member) {
    result = tryCatch(
      suppressMessages(addZenodoRelatedIdentifiers(member,
        relatedIdentifiers = isPartOf, token = token,
        sandbox = sandbox, publish = publish
      )),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      data.frame(member = as.character(member), added = 0L, published = FALSE, error = conditionMessage(result), stringsAsFactors = FALSE)
    } else {
      data.frame(member = as.character(member), added = as.integer(result$added), published = isTRUE(result$published), error = NA_character_, stringsAsFactors = FALSE)
    }
  })
  links = do.call(rbind, rows)
  failed = links[!is.na(links$error), , drop = FALSE]
  if (nrow(failed) > 0) {
    warning(paste0(
      "The IsPartOf link could not be added to ", nrow(failed), " member(s) (only records of your own Zenodo account can be changed):\n",
      paste0("- ", failed$member, ": ", failed$error, collapse = "\n")
    ), call. = FALSE)
  }
  links
}

#' Build the Zenodo deposit metadata of a collection
#' @param md named list with collection metadata
#' @param members data frame with the members, see `.collectionMembers()`
#' @param doi reserved DOI of the collection (version), or NULL
#' @param community Zenodo community identifier, or NULL
#' @return named list with the Zenodo deposit metadata
#' @noRd
.zenodoCollectionMetadata = function(md, members, doi = NULL, community = NULL) {
  people = .zenodoPeople(md$contributors, md$owner$name)

  # Suggested citation, with the DOI of this version
  # ============================================================================
  citation = md$bibliographicCitation
  if (!is.null(doi)) {
    if (is.null(citation)) {
      citation = .buildCitation(utils::modifyList(md, list(id = paste0("https://doi.org/", doi))), format(Sys.Date(), "%Y"))
    } else {
      citation = .addDoiToPackage(list(id = md$id, bibliographicCitation = citation), doi)$bibliographicCitation
    }
  } else if (is.null(citation)) {
    citation = .buildCitation(md, format(Sys.Date(), "%Y"))
  }

  # Description with the list of members (HTML)
  # ============================================================================
  memberType = if (identical(md$level, "community")) "owner collections" else "MR1 data packages (campaigns)"
  memberItems = paste0(
    "<li>", .htmlEscape(members$title),
    ifelse(is.na(members$version), "", paste0(" (version ", .htmlEscape(members$version), ")")),
    ": <a href=\"", members$conceptDoi, "\">", members$conceptDoi, "</a></li>"
  )
  description = paste(c(
    paste0("<p>", .htmlEscape(md$description), "</p>"),
    paste0("<p>This collection contains ", nrow(members), " ", memberType, ":</p>"),
    paste0("<ul>", paste(memberItems, collapse = ""), "</ul>"),
    "<p>The file <code>members.csv</code> lists the members with their DOIs, the version at the time of this version of the collection, and their period.</p>",
    paste0("<p><strong>Suggested citation:</strong> ", .htmlEscape(citation), "</p>")
  ), collapse = "\n")

  # Related identifiers: HasPart links to the members, then the others
  # ============================================================================
  hasPart = lapply(members$conceptDoi, function(d) list(identifier = d, relation = "hasPart"))
  related = .mergeZenodoRelatedIdentifiers(hasPart, .zenodoRelatedIdentifiers(md$relatedIdentifiers %||% list()))

  # Period of the members
  # ============================================================================
  starts = stats::na.omit(members$start)
  ends = stats::na.omit(members$end)
  dates = if (length(starts) > 0 && length(ends) > 0) {
    list(list(start = min(starts), end = max(ends), type = "Collected"))
  }

  # Assemble
  # ============================================================================
  licenseName = md$licenses[[1]]$name
  out = list(
    upload_type = "dataset",
    title = md$title,
    description = description,
    version = md$version,
    creators = people$creators,
    contributors = people$contributors,
    access_right = "open",
    license = unname(.zenodoLicenses[licenseName]) %||% tolower(licenseName),
    keywords = if (length(md$keywords) > 0) I(unlist(md$keywords)),
    related_identifiers = related,
    dates = dates,
    communities = if (!is.null(community)) list(list(identifier = community)),
    prereserve_doi = TRUE
  )
  out[!vapply(out, is.null, logical(1))]
}
