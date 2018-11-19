#' Web of Science Endpoints
#'
#' @param api The API being used
#' @param out_type output type, either \code{json} or \code{json}
#'
#' @return A list of endpoints for each API
#' @export
#'
#' @examples
#' ws_endpoints()
#' ws_endpoints(out_type = "xml")
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
  incites = .ws_incites_endpoints()
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





