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
#' tfile = tempfile()
#' writeLines("my_api_key", con = tfile)
#' options(ws_incites_key_filename = tfile)
#' ws_incites_key()
#' file.remove(tfile)
#' options(ws_incites_key_filename = NULL)
#'
#' ws_filename = getOption("ws_incites_key_filename")
#' INCITES_KEY = Sys.getenv("INCITES_KEY")
#' Sys.setenv(INCITES_KEY = "")
#' writeLines(c("my_api_key1", "api_key2"), con = tfile)
#' options(ws_incites_key_filename = tfile)
#' testthat::expect_warning(ws_incites_key())
#' file.remove(tfile)
#' Sys.setenv(INCITES_KEY = INCITES_KEY)
#' options(ws_incites_key_filename = ws_filename)
ws_incites_key = function(api_key = NULL, error = TRUE) {
  api_key = ws_api_key(api_key = api_key, error = error,
             api = "incites")
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
#' @examples
#' ws_set_incites_key("my_incites_api_key")
ws_set_incites_key = function(api_key) {
  x = ws_list_apis()
  x = x$incites
  ws_set_key_option(api_key = api_key, key_option =  x$key_option)
}


#' @rdname ws_endpoints
#' @export
#' @examples
#' ws_incites_endpoints()
ws_incites_endpoints = function(out_type = c("json", "xml")) {
  ws_endpoints(out_type, api = "incites")
}

#' @rdname ws_endpoints
.ws_incites_endpoints = function() {
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
