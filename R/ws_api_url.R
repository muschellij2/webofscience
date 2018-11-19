#' Web of Science API URL
#'
#' @param api an endpoint for the API, see
#' \url{https://developer.clarivate.com/apis}
#'
#' @return A character path of the URL
#' @export
#'
#' @examples
#' ws_api_url()
#' ws_incites_url()
#' ws_api_url(api = "incites")
#' ws_endnote_url()
#' ws_tipms_url()
#' ws_wos_url()
#' ws_woslite_url()
ws_api_url = function(api = NULL) {
  paste0("https://api.clarivate.com/api/", api)
}

#' @rdname ws_api_url
#' @export
ws_incites_url = function() {
  ws_api_url("incites")
}

#' @rdname ws_api_url
#' @export
ws_endnote_url = function() {
  ws_api_url(c("endnote", "v1"))
}

#' @rdname ws_api_url
#' @export
ws_tipms_url = function() {
  ws_api_url(c("tipms", "api", "v1"))
}

#' @rdname ws_api_url
#' @export
ws_wos_url = function() {
  ws_api_url("wos")
}

#' @rdname ws_api_url
#' @export
ws_woslite_url = function() {
  ws_api_url("woslite")
}
