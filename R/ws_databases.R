#' Web of Science Databases
#'
#' @return character vector
#' @export
#'
#' @examples
#' ws_databases()
#' ws_db()
ws_databases = function() {
  c( "WOK", "BCI", "BIOABS", "BIOSIS", "CABI", "CCC", "CSCD", "DCI", "DIIDW",
     "FSTA", "INSPEC", "KJD", "MEDLINE", "RSCI", "SCIELO",
     "WOS", "ZOOREC")
}

#' @rdname ws_databases
#' @export
ws_db = ws_databases
