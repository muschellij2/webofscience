#' @title Find API Key for The IP Management System
#'
#' @description Determines if \code{option(ws_tipms_key)} or
#' \code{option(ws_tipms_key_filename)} is set.
#' If not, it stops and returns an error.  If so, returns the value.
#'
#' @inheritParams ws_api_key
#' @export
#' @examples
#' res = ws_tipms_key(error = FALSE)
ws_tipms_key = function(api_key = NULL, error = TRUE) {
  x = ws_list_apis()
  x = x$tipms
  api_key = ws_api_key(
    api_key = api_key,
    error = error,
    key_option = x$key_option,
    sys_env = x$sys_env,
    key_filename_option = x$key_filename_option,
    api_name = x$api_name)
  return(api_key)
}

#' @rdname ws_tipms_key
#' @export
#' @examples
#' ws_have_tipms_key()
ws_have_tipms_key = function(api_key = NULL) {
  api_key = ws_tipms_key(api_key = api_key, error = FALSE)
  !is.null(api_key)
}


#' @rdname ws_set_key_option
#' @export
#' @examples
#' ws_tipms_key = getOption("ws_tipms_key")
#' ws_set_tipms_key(ws_tipms_key)
ws_set_tipms_key = function(api_key) {
  x = ws_list_apis()
  x = x$tipms
  ws_set_key_option(api_key = api_key, key_option = x$key_option)
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
