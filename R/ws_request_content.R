#' Get Content from Request
#'
#' @param request Request from \code{\link{VERB}}
#' @param error Should errors (compared to warnings)
#' be issued
#' @param ... additional arguments to send to \code{\link{fromJSON}}
#'
#' @return An list from \code{\link{jsonlite}}
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom httr stop_for_status content warn_for_status
ws_request_content = function(
  request,
  error = FALSE,
  ...
) {
  if (error) {
    httr::stop_for_status(request)
  } else {
    httr::warn_for_status(request)
  }
  cr = httr::content(request, as = "text", encoding = "UTF-8")
  cr = jsonlite::fromJSON(txt = cr, ...)
  return(cr)
}
