#' Build Request for Web of Science
#'
#' @param api_key API key for the
#' @param api API to be used
#' @param endpoint endpoint for that specific API
#' @param ... additional arguments to send to \code{\link{VERB}}
#'
#' @return A request output
#' @export
#' @importFrom httr VERB accept_json
#'
#' @examples
#' ws_build_request(api_key = ws_incites_key(error = FALSE),
#' api = "incites", endpoint = "InCitesLastUpdated")
ws_build_request = function(
  api_key = NULL,
  api,
  endpoint,
  ...) {
  ep = ws_endpoints(
    out_type = "json",
    api = api)
  endpoint = match.arg(endpoint,
                 choices = names(ep))
  ep = ep[[endpoint]]

  url = ws_api_url(api = api)
  url = paste0(url, ep$endpoint)

  api_key = ws_api_key(api_key = api_key, api = api, error = FALSE)
  api_hdr = ws_api_key_header(api_key = api_key)

  res = httr::VERB(
    verb = ep$verb,
    url = url,
    ...,
    httr::accept_json(),
    api_hdr)
  return(res)
}
