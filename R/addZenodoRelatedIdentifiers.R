#' @title Add Related Identifiers to a Zenodo Record
#' @author Birgen Haest
#' @description [addZenodoRelatedIdentifiers()] adds related identifiers to the
#' metadata of a Zenodo record, e.g. an `IsPartOf` link from a campaign data
#' package to the owner collection it belongs to, or a `HasPart` link from a
#' collection to a campaign. Related identifiers that the record already has are
#' not added twice; existing ones are kept.
#'
#' Only the metadata are changed: the files, the DOI and the version of the
#' record stay the same, so no new version is needed. The latest version of the
#' record is updated, whichever version's DOI (or the concept DOI) is given.
#'
#' * For a **published** record, the function opens an edit of the record and
#'   updates its metadata. With `publish = FALSE` (default), the edit stays
#'   open: check it on Zenodo and publish it there (or discard it). With
#'   `publish = TRUE`, the edit is published directly.
#' * For an **unpublished draft**, the metadata of the draft are updated. The
#'   draft is never published, also not with `publish = TRUE`.
#' @param doi DOI (e.g. `"https://doi.org/10.5281/zenodo.1234567"` or
#' `"10.5281/zenodo.1234567"`) or record ID of the Zenodo record.
#' @param relatedIdentifiers related identifiers to add: a list of lists, each
#' with `relationType` (DataCite relation type, e.g. `"IsPartOf"`, `"HasPart"`,
#' `"IsCitedBy"`) and `relatedIdentifier` (e.g. a DOI URL), as in the metadata
#' file of a data package; or a data frame with these two columns.
#' @param token Zenodo personal access token with the scopes `deposit:write`
#' and `deposit:actions`. Default is the environment variable `ZENODO_TOKEN`
#' (or `ZENODO_SANDBOX_TOKEN` if `sandbox = TRUE`). Never write the token in a
#' script.
#' @param sandbox logical. If `TRUE`, use the Zenodo sandbox
#' (<https://sandbox.zenodo.org>). Default is `FALSE`.
#' @param publish logical. If `TRUE`, the edit of a published record is
#' published directly. Default is `FALSE`: the edit stays open on Zenodo.
#'
#' @return A list, invisibly, with `id` (Zenodo record ID), `url` (link to the
#' record on Zenodo), `added` (number of related identifiers added) and
#' `published` (logical, whether the edit was published).
#' @family write file functions
#' @export
#' @examples
#' \dontrun{
#' # Link a campaign data package to its owner collection (Zenodo sandbox)
#' # ===========================================================================
#' addZenodoRelatedIdentifiers(
#'   doi = "https://doi.org/10.5281/zenodo.1234567",
#'   relatedIdentifiers = list(list(
#'     relationType      = "IsPartOf",
#'     relatedIdentifier = "https://doi.org/10.5281/zenodo.7654321"
#'   )),
#'   sandbox = TRUE
#' )
#' }
addZenodoRelatedIdentifiers = function(doi,
                                       relatedIdentifiers,
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
  if (!(is.list(relatedIdentifiers) || is.data.frame(relatedIdentifiers)) || length(relatedIdentifiers) == 0) {
    stop("'relatedIdentifiers' must be a non-empty list of lists, or a data frame, with 'relationType' and 'relatedIdentifier'.")
  }
  newRelated = .zenodoRelatedIdentifiers(relatedIdentifiers)
  for (ri in newRelated) {
    if (length(ri$identifier) != 1 || !nzchar(ri$identifier) || length(ri$relation) != 1 || !nzchar(ri$relation)) {
      stop("Each related identifier needs a 'relationType' and a 'relatedIdentifier'.")
    }
  }
  token = .zenodoToken(token, sandbox)

  # Get the (latest version of the) record
  # ==============================================================================
  baseUrl = .zenodoBaseUrl(sandbox)
  recordId = .zenodoLatestRecordId(recordId, baseUrl, token)
  depositionUrl = paste0(baseUrl, "/api/deposit/depositions/", recordId)
  deposition = .zenodoRequest(depositionUrl, token)
  recordUrl = deposition$links$html %||% paste0(baseUrl, "/records/", recordId)
  published = isTRUE(deposition$submitted)

  # Merge the related identifiers
  # ==============================================================================
  current = deposition$metadata$related_identifiers %||% list()
  merged = .mergeZenodoRelatedIdentifiers(current, newRelated)
  nAdded = length(merged) - length(current)
  if (nAdded == 0) {
    message(paste0("Zenodo record ", recordId, " already has all these related identifiers; nothing changed."))
    return(invisible(list(id = recordId, url = recordUrl, added = 0L, published = FALSE)))
  }

  # Open an edit of a published record (not needed if an edit is already open)
  # ==============================================================================
  editOpened = FALSE
  if (published && !identical(deposition$state, "inprogress")) {
    .zenodoRequest(paste0(depositionUrl, "/actions/edit"), token, method = "POST")
    editOpened = TRUE
  }

  # Update the metadata, and publish the edit if requested
  # ==============================================================================
  metadata = deposition$metadata
  metadata$related_identifiers = merged
  if (published) {
    # The DOI of a published record is registered and kept by Zenodo: do not
    # send it (Zenodo would take it for an external DOI) nor ask for a new one
    metadata$doi = NULL
    metadata$prereserve_doi = NULL
  } else if (!is.null(metadata$prereserve_doi)) {
    metadata$prereserve_doi = TRUE
  }
  tryCatch(
    {
      .zenodoRequest(depositionUrl, token, method = "PUT", json = list(metadata = metadata))
      if (published && publish) {
        .zenodoRequest(paste0(depositionUrl, "/actions/publish"), token, method = "POST")
      }
    },
    error = function(e) {
      if (editOpened) {
        tryCatch(.zenodoRequest(paste0(depositionUrl, "/actions/discard"), token, method = "POST"),
          error = function(e2) NULL
        )
      }
      stop(paste0(
        conditionMessage(e), " The related identifiers of Zenodo record ", recordId, " were not changed."
      ), call. = FALSE)
    }
  )

  editPublished = published && publish
  message(paste0(
    nAdded, " related identifier(s) added to Zenodo record ", recordId,
    if (sandbox) " (sandbox)", ": ", recordUrl,
    if (published && !publish) "\nThe edit is not published yet: check it on Zenodo and publish it there.",
    if (!published) "\nThe record is an unpublished draft: the related identifiers will be published with it."
  ))
  invisible(list(id = recordId, url = recordUrl, added = nAdded, published = editPublished))
}
