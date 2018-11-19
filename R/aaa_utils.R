
#' Return Numeric of Current year
#'
#' @rdname current_year
#' @export
#' @return A scalar numeric
current_year = function() {
  as.numeric(format(Sys.Date(), "%Y"))
}


