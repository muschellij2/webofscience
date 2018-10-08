#' Check API Keys for Various Web of Science APIs
#'
#' @param api_key Web of Science API key
#' @param error Should the function error if \code{api_key = NULL} after
#' looking in various places?  (See notes)
#' @param key_option option to grab API key using \code{\link{getOption}}
#' @param sys_env System environment variable to grab API key from
#' \code{\link{Sys.getenv}}
#' @param key_filename_option Option retrieved from \code{\link{getOption}},
#' which is a location of a file with the API key.
#' @param api_name Name of API key
#'
#' @note You can either set the API key using
#' \code{option(ws_API_key)} or have it accessible by
#' \code{api_key = Sys.getenv('API_KEY')}, where \code{API}
#' is one of the Web of Science APIs, such as \code{INCITES}
#'
#' @return API key character string
#' @export
#'
#' @examples
#' api_key = NULL
#' error = FALSE
#'   api_key = ws_api_key(
#'   api_key = api_key,
#'   error = error,
#'   key_option = "ws_incites_key",
#'   sys_env = "INCITES_KEY",
#'   key_filename_option = "ws_incites_key_filename",
#'   api_name = "Incites")
ws_api_key = function(
  api_key = NULL, error = TRUE,
  key_option,
  sys_env,
  key_filename_option,
  api_name = "") {

  if (is.null(api_key)) {
    api_key = getOption(key_option)
  }
  if (is.null(api_key)) {
    api_key = Sys.getenv(sys_env)
    if (api_key %in% "") {
      api_key = NULL
    }
  }

  if (is.null(api_key)) {
    api_key_filename = getOption(key_filename_option)
    if (!is.null(api_key_filename)) {
      api_key = readLines(api_key_filename)
    }
    if (length(api_key) > 1) {
      warning(paste0(api_name, " API key from ", api_key_filename,
                     " had too many lines! Taking first \n"))
      api_key = api_key[1]
    }
  }
  if (!is.null(api_key)) {
    if (api_key %in% "") {
      api_key = NULL
    }
  }

  if (is.null(api_key) & error) {
    stop(paste0(api_name, " API key not found, please set ",
                "option('", key_filename_option, "') or ",
                "option('", key_option, "') for general use or ",
                "set environment variable ", sys_env, ", to be ",
                "accessed by Sys.getenv('", sys_env, "')"))
  }
  return(api_key)
}

#' @rdname ws_api_key
#' @export
#' @examples
#' ws_list_apis()
ws_list_apis = function() {
  L = list(incites = list(    key_option = "ws_incites_key",
                          sys_env = "INCITES_KEY",
                          key_filename_option = "ws_incites_key_filename",
                          api_name = "Incites",
                          endpoint = "incites"),
       endnote = list(    key_option = "ws_endnote_key",
                          sys_env = "ENDNOTE_KEY",
                          key_filename_option = "ws_endnote_key_filename",
                          api_name = "EndNote",
                          endpoint = "endnote"),
       wos = list(    key_option = "ws_wos_key",
                          sys_env = "WOS_KEY",
                          key_filename_option = "ws_wos_key_filename",
                          api_name = "WOS",
                          endpoint = "wos"),
       woslite = list(    key_option = "ws_woslite_key",
                          sys_env = "WOSLITE_KEY",
                          key_filename_option = "ws_woslite_key_filename",
                          api_name = "WOS Lite",
                          endpoint = "woslite"),
       tipms = list(    key_option = "ws_tipms_key",
                          sys_env = "TIPMS_KEY",
                          key_filename_option = "ws_tipms_key_filename",
                          api_name = "IP Management System",
                          endpoint = "tipms")
       )
  L = lapply(L, function(x) {
    x$url = ws_api_url(x$endpoint)
    x
  })
  return(L)
}

#' @rdname ws_api_key
#' @export
#' @importFrom httr add_headers
#' @examples
#' ws_auth_header(api_key = "my_api_key")
ws_auth_header = function(api_key = NULL) {
  httr::add_headers('X-API-Key' = api_key)
}

#' @title Set API Key for API endpoint
#'
#' @description Sets API key using
#' if \code{options(key_option)}
#' @param api_key Web of Science API key for specific endpoint
#' @param key_option option to set API key using \code{\link{options}}
#' @return NULL
#' @export
#' @examples
#' ws_set_key_option(api_key = "my_api_key", key_option = "ws_incites_key")
ws_set_key_option = function(api_key, key_option) {
  options(key_option = api_key)
  invisible(NULL)
}



