#' @title Find API Key for Web of Science
#'
#' @description Determines if \code{option(ws_wos_key)} or
#' \code{option(ws_wos_key_filename)} is set.
#' If not, it stops and returns an error.  If so, returns the value.
#'
#' @inheritParams ws_api_key
#' @export
#' @examples
#' res = ws_wos_key(error = FALSE)
ws_wos_key = function(api_key = NULL, error = TRUE) {
  x = ws_list_apis()
  x = x$wos
  api_key = ws_api_key(
    api_key = api_key,
    error = error,
    key_option = x$key_option,
    sys_env = x$sys_env,
    key_filename_option = x$key_filename_option,
    api_name = x$api_name)
  return(api_key)
}

#' @rdname ws_wos_key
#' @export
#' @examples
#' ws_have_wos_key()
ws_have_wos_key = function(api_key = NULL) {
  api_key = ws_wos_key(api_key = api_key, error = FALSE)
  !is.null(api_key)
}


#' @rdname ws_set_key_option
#' @export
#' @examples
#' ws_wos_key = getOption("ws_wos_key")
#' ws_set_wos_key(ws_wos_key)
ws_set_wos_key = function(api_key) {
  x = ws_list_apis()
  x = x$wos
  ws_set_key_option(api_key = api_key, key_option = x$key_option)
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



