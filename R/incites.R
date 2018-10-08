#' @title Find API Key for Incites
#'
#' @description Determines if \code{option(ws_incites_key)} or
#' \code{option(ws_incites_key_filename)} is set.
#' If not, it stops and returns an error.  If so, returns the value.
#'
#' @inheritParams ws_api_key
#' @export
#' @examples
#' res = ws_incites_key(error = FALSE)
ws_incites_key = function(api_key = NULL, error = TRUE) {
  x = ws_list_apis()
  x = x$incites
  api_key = ws_api_key(
    api_key = api_key,
    error = error,
    key_option = x$key_option,
    sys_env = x$sys_env,
    key_filename_option = x$key_filename_option,
    api_name = x$api_name)
  return(api_key)
}

#' @rdname ws_incites_key
#' @export
#' @examples
#' ws_have_incites_key()
ws_have_incites_key = function(api_key = NULL) {
  api_key = ws_incites_key(api_key = api_key, error = FALSE)
  !is.null(api_key)
}


#' @rdname ws_set_key_option
#' @export
ws_set_incites_key = function(api_key) {
  x = ws_list_apis()
  x = x$incites
  ws_set_key_option(api_key = api_key, key_option =  x$key_option)
}


#' @rdname ws_endpoints
#' @export
#' @examples
#' ws_incites_endpoints()
ws_incites_endpoints = function() {
  df = list(
    "DocumentLevelMetricsByInstitutionId" = list(
      description = "Get metrics by institution Id",
      verb = "GET"
    ),
    "DocumentLevelMetricsByInstitutionIdRecordCount" =
      list(
        description = "Get metrics count by year",
        verb = "GET"
      ),
    "DocumentLevelMetricsByUT" =
      list(
        description = "Get metrics by UT",
        verb = "GET"
      ),
    "CheckValidUTs" =
      list(
        description = "Get invalid UTs",
        verb = "GET"
      ),
    "InCitesLastUpdated" =
      list(
        description = "Get last updated information",
        verb = "GET"
      ))
  return(df)
}
