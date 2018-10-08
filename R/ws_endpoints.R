#' Web of Science Endpoints
#'
#' @param api The API being used
#' @param out_type output type, either \code{json} or \code{json}
#'
#' @return A list of endpoints for each API
#' @export
#'
#' @examples
#' ws_endpoints(api = "incites")
#' ws_endpoints(api = c("incites", "endnote"))
#' ws_incites_endpoints()
#' ws_endnote_endpoints()
#' ws_woslite_endpoints()
#' ws_woslite_endpoints()
#' ws_tipms_endpoints()
ws_endpoints = function(
  out_type = c("json", "xml"),
  api = NULL
) {
  out_type = match.arg(out_type)

  add_endpoints = function(df ) {
    df = mapply(function(x, name) {
      x$mime_type = out_type
      x$endpoint = file.path("", name, out_type)
      x
    }, df, names(df), SIMPLIFY = FALSE)
  }
  incites = ws_incites_endpoints()
  incites = add_endpoints(incites)

  endpoint = ws_endnote_endpoints()
  woslite = ws_woslite_endpoints()
  wos = ws_wos_endpoints()
  tipms = ws_tipms_endpoints()

  L = list(incites = incites,
           endpoint = endpoint,
           woslite = woslite,
           wos = wos,
           tipms = tipms)
  if (!is.null(api)) {
    if (length(api) == 1) {
      return(L[[api]])
    }
    return(L[api])
  }
  return(L)
}




#' @rdname ws_endpoints
#' @export
ws_woslite_endpoints = function() {
  L = list(
    root = list(
      verb = "GET",
      endpoint = "/",
      description = "  Submits a user query and returns results"
    ),
    query = list(
      verb = "GET",
      endpoint = "/query/{queryId}",
      description = "Fetch record(s) by query identifier"
    ),
    id = list(
      endpoint = "/id/{uniqueId}",
      verb = "GET",
      description = "Find record(s) by specific id")
  )
  return(L)
}


#' @rdname ws_endpoints
#' @export
ws_wos_endpoints = function() {
  L = list(
    id = list(
      verb = "GET",
      endpoint = "/",
      description = "Submits a user query and returns results"),
    query = list(
      verb = "GET",
      endpoint = "/query/{queryId}",
      description = "Fetch record(s) by query identifier"),
    related = list(
      verb = "GET",
      endpoint = "/related",
      description = "Identify related records"),
    citing = list(
      verb = "GET",
      endpoint = "/citing",
      description = "Find citing items"),
    references = list(
      verb = "GET",
      endpoint = "/references",
      description = "Find cited references by id"),
    id = list(
      verb = "GET",
      endpoint = "/id/{uniqueId}",
      description = "Find record(s) by specific id"),
    recordids = list(
      verb = "GET",
      endpoint = "/recordids/{queryId}",
      description = "Return IDs of records for a given query by a query identifier")
  )
  return(L)
}


#' @rdname ws_endpoints
#' @export
ws_tipms_endpoints = function() {
  L = list(
    case_record_post = list(
      verb = "POST",
      endpoint = "/CaseRecord/{ipType}",
      description = "Posts (Creates) the fields for case record."),
    case_record_put = list(
      verb = "PUT",
      endpoint = "/CaseRecord/{ipType}/{masterId}",
      description = "Puts (Updates) the fields for specified case record."),
    file = list(
      verb = "POST",
      endpoint = "/File",
      description = "Posts selected file."),
    health = list(
      verb = "GET",
      endpoint = "/Health",
      description = "Reflects the state of the application."),
    patent_filing_forms = list(
      verb = "GET",
      endpoint = "/patentfilingforms",
      description = "Gets (Retrieves) the patent filing forms data."),
    tempfile = list(
      verb = "POST",
      endpoint = "/TempFile",
      description = "Posts (Creates) the selected file."),
    trademark_reporting = list(
      verb = "GET",
      endpoint = "/trademarkreporting",
      description = "Search for Trademarks reporting data.")
  )
  return(L)
}
