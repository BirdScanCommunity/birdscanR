# Internal helpers for the Zenodo REST API, shared by the functions that upload,
# update and read MR1 data packages on Zenodo.

#' Check that httr2 is installed
#' @noRd
.checkHttr2 = function() {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("The package 'httr2' is required for Zenodo. Install it with install.packages(\"httr2\").")
  }
  invisible(TRUE)
}

#' Base URL of Zenodo or the Zenodo sandbox
#' @noRd
.zenodoBaseUrl = function(sandbox) {
  if (sandbox) "https://sandbox.zenodo.org" else "https://zenodo.org"
}

#' Get the Zenodo token from the argument or the environment
#' @noRd
.zenodoToken = function(token, sandbox) {
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
  token
}

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
  if (!httr2::resp_has_body(resp) || length(httr2::resp_body_raw(resp)) == 0) {
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

#' Get the Zenodo record ID from a DOI, DOI URL, Zenodo URL or record ID
#' @param x e.g. "https://doi.org/10.5281/zenodo.123", "10.5281/zenodo.123",
#' "https://zenodo.org/records/123" or 123
#' @return record ID (integer)
#' @noRd
.zenodoRecordId = function(x) {
  if (is.numeric(x) && length(x) == 1 && !is.na(x)) {
    return(as.integer(x))
  }
  if (!is.character(x) || length(x) != 1) {
    stop("A Zenodo DOI or record ID must be a single value.")
  }
  if (grepl("^[0-9]+$", x)) {
    return(as.integer(x))
  }
  id = regmatches(x, regexec("zenodo\\.([0-9]+)$|zenodo\\.org/(?:records|record|deposit|uploads)/([0-9]+)", x, perl = TRUE))[[1]]
  if (length(id) == 0) {
    stop(paste0("Not a Zenodo DOI or record ID: ", x))
  }
  as.integer(id[nzchar(id)][2])
}

#' Is x a Zenodo DOI, Zenodo URL or record ID?
#' @noRd
.isZenodoId = function(x) {
  !is.null(x) && !inherits(tryCatch(.zenodoRecordId(x), error = function(e) e), "error")
}

#' Get the record ID of the latest published version of a Zenodo record
#' @description Works with the DOI or record ID of any version, and with the
#' concept DOI or record ID. If the record is not published (yet), the record ID
#' itself is returned.
#' @noRd
.zenodoLatestRecordId = function(x, baseUrl, token) {
  recordId = .zenodoRecordId(x)
  latest = tryCatch(
    .zenodoRequest(paste0(baseUrl, "/api/records/", recordId, "/versions/latest"), token),
    error = function(e) NULL
  )
  if (is.null(latest$id)) recordId else as.integer(latest$id)
}

#' Convert related identifiers to Zenodo notation
#' @param relatedIdentifiers list of lists with `relationType` (DataCite, e.g.
#' "IsPartOf") and `relatedIdentifier`, or a data frame with these columns
#' @return list of lists with `identifier` and `relation` (e.g. "isPartOf")
#' @noRd
.zenodoRelatedIdentifiers = function(relatedIdentifiers) {
  if (is.data.frame(relatedIdentifiers)) {
    relatedIdentifiers = lapply(seq_len(nrow(relatedIdentifiers)), function(i) as.list(relatedIdentifiers[i, , drop = FALSE]))
  }
  lapply(relatedIdentifiers, function(ri) {
    relation = as.character(ri$relationType)
    list(
      identifier = as.character(ri$relatedIdentifier),
      relation = paste0(tolower(substr(relation, 1, 1)), substring(relation, 2))
    )
  })
}

#' Key to compare related identifiers, ignoring case and DOI URL prefixes
#' @noRd
.relatedIdentifierKey = function(ri) {
  identifier = sub("^https?://(dx\\.)?doi\\.org/", "", ri$identifier %||% "")
  paste(tolower(ri$relation %||% ""), tolower(identifier))
}

#' Merge Zenodo related identifiers
#' @description Returns `current`, followed by the entries of `additional` that
#' are not in `current` yet.
#' @noRd
.mergeZenodoRelatedIdentifiers = function(current, additional) {
  current = current %||% list()
  keys = vapply(current, .relatedIdentifierKey, character(1))
  for (ri in additional %||% list()) {
    key = .relatedIdentifierKey(ri)
    if (!key %in% keys) {
      current[[length(current) + 1]] = ri
      keys = c(keys, key)
    }
  }
  current
}
