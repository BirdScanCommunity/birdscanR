# Helpers to mock the Zenodo API in tests

base = "https://sandbox.zenodo.org"

# Mock of the Zenodo API: `routes` is a list of handlers named
# "<METHOD> <url regex>", each a function(req) returning a list (JSON body) or
# an httr2 response. All requests are recorded in `log`.
mockRouter = function(log, routes) {
  function(req) {
    log$requests[[length(log$requests) + 1]] = list(method = req$method, url = req$url, body = req$body$data)
    for (route in names(routes)) {
      parts = strsplit(route, " ", fixed = TRUE)[[1]]
      if (req$method == parts[1] && grepl(parts[2], req$url)) {
        out = routes[[route]](req)
        if (inherits(out, "httr2_response")) {
          return(out)
        }
        return(httr2::response_json(status_code = 200, body = out))
      }
    }
    stop(paste("Unexpected request:", req$method, req$url))
  }
}
newLog = function() {
  log = new.env()
  log$requests = list()
  log
}
requestSummary = function(log) {
  vapply(log$requests, function(r) paste(r$method, sub(base, "", r$url, fixed = TRUE)), character(1))
}
