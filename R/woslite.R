#' @title Find API Key for WOS Lite
#'
#' @description Determines if \code{option(ws_woslite_key)} or
#' \code{option(ws_woslite_key_filename)} is set.
#' If not, it stops and returns an error.  If so, returns the value.
#'
#' @inheritParams ws_api_key
#' @export
#' @examples
#' res = ws_woslite_key(error = FALSE)
ws_woslite_key = function(api_key = NULL, error = TRUE) {
  x = ws_list_apis()
  x = x$woslite
  api_key = ws_api_key(
    api_key = api_key,
    error = error,
    key_option = x$key_option,
    sys_env = x$sys_env,
    key_filename_option = x$key_filename_option,
    api_name = x$api_name)
  return(api_key)
}

#' @rdname ws_woslite_key
#' @export
#' @examples
#' ws_have_woslite_key()
ws_have_woslite_key = function(api_key = NULL) {
  api_key = ws_woslite_key(api_key = api_key, error = FALSE)
  !is.null(api_key)
}


#' @rdname ws_set_key_option
#' @export
#' @examples
#' ws_woslite_key = getOption("ws_woslite_key")
#' ws_set_woslite_key(ws_woslite_key)
ws_set_woslite_key = function(api_key) {
  x = ws_list_apis()
  x = x$woslite
  ws_set_key_option(api_key = api_key, key_option = x$key_option)
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

