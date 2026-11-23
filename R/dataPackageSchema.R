#' Return y if x is NULL (base R has this only since R 4.4.0)
#' @noRd
`%||%` = function(x, y) if (is.null(x)) y else x

#' Read the field definitions of the MR1 data package resources
#' @return data frame with columns resource, name, pattern, description, unit
#' @noRd
.readFieldDefinitions = function() {
  filePath = system.file("extdata", "dataPackage", "fieldDefinitions.csv",
    package = "birdscanR"
  )
  fieldDefs = utils::read.csv(filePath,
    colClasses = "character", stringsAsFactors = FALSE,
    encoding = "UTF-8"
  )
  fieldDefs$pattern = as.logical(fieldDefs$pattern)
  return(fieldDefs)
}

#' Primary and foreign keys of the MR1 data package resources
#' @noRd
.mr1dpKeys = list(
  site_data = list(primaryKey = "siteID", foreignKeys = NULL),
  protocol_data = list(
    primaryKey = "protocolID",
    foreignKeys = list(list(
      fields = "siteID",
      reference = list(resource = "site_data", fields = "siteID")
    ))
  ),
  echo_data = list(
    primaryKey = "echo",
    foreignKeys = list(list(
      fields = "protocolID",
      reference = list(resource = "protocol_data", fields = "protocolID")
    ))
  ),
  blind_times = list(
    primaryKey = NULL,
    foreignKeys = list(list(
      fields = "protocolID",
      reference = list(resource = "protocol_data", fields = "protocolID")
    ))
  ),
  sunrise_sunset = list(primaryKey = NULL, foreignKeys = NULL)
)

#' Build the Table Schema of an MR1 data package resource
#' @description Derives field types from the data with
#' `frictionless::create_schema()`, then adds a description and unit to each
#' field from the field definitions (exact name first, then regex patterns),
#' and adds the primary and foreign keys of the resource.
#' @param data data frame of the resource
#' @param resourceName name of the resource, e.g. "echo_data"
#' @param fieldDefs field definitions, see `.readFieldDefinitions()`
#' @return list with the Table Schema
#' @noRd
.buildResourceSchema = function(data, resourceName, fieldDefs) {
  schema = frictionless::create_schema(data)
  defs = fieldDefs[fieldDefs$resource == resourceName, ]
  exactDefs = defs[!defs$pattern, ]
  patternDefs = defs[defs$pattern, ]

  # Add description and unit to each field
  # ============================================================================
  undocumented = character(0)
  for (i in seq_along(schema$fields)) {
    fieldName = schema$fields[[i]]$name
    def = exactDefs[exactDefs$name == fieldName, ]
    if (nrow(def) == 0) {
      matches = vapply(patternDefs$name, grepl, logical(1), x = fieldName)
      def = patternDefs[matches, ][1, ]
    }
    if (nrow(def) == 0 || is.na(def$description)) {
      undocumented = c(undocumented, fieldName)
      schema$fields[[i]]$description = "No description available."
    } else {
      schema$fields[[i]]$description = def$description
      if (nzchar(def$unit)) schema$fields[[i]]$unit = def$unit
    }
  }
  if (length(undocumented) > 0) {
    warning(paste0(
      "No description available for the following column(s) of '", resourceName,
      "': ", paste(undocumented, collapse = ", ")
    ))
  }

  # Add primary and foreign keys
  # ============================================================================
  keys = .mr1dpKeys[[resourceName]]
  fieldNames = vapply(schema$fields, function(f) f$name, character(1))
  if (!is.null(keys$primaryKey) && all(keys$primaryKey %in% fieldNames)) {
    schema$primaryKey = I(keys$primaryKey)
  }
  if (!is.null(keys$foreignKeys)) {
    fks = Filter(function(fk) all(fk$fields %in% fieldNames), keys$foreignKeys)
    fks = lapply(fks, function(fk) {
      fk$fields = I(fk$fields)
      fk$reference$fields = I(fk$reference$fields)
      fk
    })
    if (length(fks) > 0) schema$foreignKeys = fks
  }
  return(schema)
}
