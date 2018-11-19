
#' Get Incites Documents by Institution ID
#'
#' @param year Your response will contain records published
#' starting in this year. Must be at least 1980
#' @param record_count This is the maximum number of records that
#' will be returned per request. Must be less than the
#' limit placed on the developer key (default maximum is 100)
#' @param starting_record Starting index of Records returned.
#' Used when number of Records required exceeds limit
#' on single request. Starts at Index 1
#' @param esci Flag to return InCites baseline metrics with
#' Emerging Sources Citation Index (ESCI) content.
#' Accepts y/n case insensitive values only.
#' y - returns InCites metrics with ESCI index,
#' n - returns InCites metrics without ESCI index
#' @param ... additional arguments to send to \code{\link{fromJSON}}
#' @param api_key InCites Web of Science API key
#'
#' @return A list of the request and the content
#' @export
#'
#' @rdname ws_incites_by_inst_id
#' @examples
#' if (ws_have_incites_key()) {
#' result = ws_incites_by_inst_id(year = current_year(),
#' flatten = TRUE)
#' docs = result$content$api$rval[[1]]
#' result_with_esci = ws_incites_by_inst_id(year = current_year(),
#' flatten = TRUE, esci = "y")
#' esci_docs = result_with_esci$content$api$rval[[1]]
#' }
ws_incites_by_inst_id = function(
  year = 2018,
  record_count = 100,
  starting_record = 1,
  esci = c("n", "y"),
  api_key = NULL,
  ...) {

  if (is.logical(esci)) {
    esci = c("n", "y")[ esci + 1 ]
  } else {
    esci = tolower(esci)
    esci = match.arg(esci)
  }
  year = as.numeric(year)
  stopifnot(!is.na(year))
  if (year < 1980) {
    stop("year must be >= 1980")
  }

  query = list()
  query$year = year
  query$recordcount = record_count
  query$startingrecord = starting_record
  query$esci = esci

  res = ws_build_request(
    api_key = api_key,
    api = "incites",
    endpoint = "DocumentLevelMetricsByInstitutionId",
    query = query)
  cr = ws_request_content(res, error = FALSE, ...)

  L = list(
    response = res,
    content = cr)
  class(L) = "ws_result"
  return(L)
}


#' @rdname ws_incites_by_inst_id
#' @export
#' @examples
#' if (ws_have_incites_key()) {
#' rec = ws_incites_by_record_count(year = current_year(),
#' flatten = TRUE)
#' rec$content$api$rval
#' }
ws_incites_by_record_count = function(
  year = 2018,
  api_key = NULL,
  ...) {

  year = as.numeric(year)
  stopifnot(!is.na(year))
  if (year < 1980) {
    stop("year must be >= 1980")
  }

  query = list()
  query$year = year

  res = ws_build_request(
    api_key = api_key,
    api = "incites",
    endpoint = "DocumentLevelMetricsByInstitutionIdRecordCount",
    query = query)
  cr = ws_request_content(res, error = FALSE, ...)

  L = list(
    response = res,
    content = cr)
  class(L) = "ws_result"
  return(L)
}


#' @param ut a vector of UT values and get InCites metrics in
#' the response. A valid UT is 15 alphanumeric digits.
#' All trailing 0’s must be left in the UT, and there
#' should be no spaces between UTs. If there is no data
#' for a UT or an invalid UT is entered, that UT will be
#' ignored in the response. A limit of 100 UTs may be submitted
#' per request. To see sample API results, use one or more
#'  of the following UTs
#' @export
#' @rdname ws_incites_by_inst_id
#' @examples
#' ut = c("000352040700014","000353267900023",
#'  "000346982900018","000346342200004")
#' if (ws_have_incites_key()) {
#' result = ws_incites_by_ut(ut = ut,
#' flatten = TRUE)
#' docs = result$content$api$rval[[1]]
#' }
ws_incites_by_ut = function(
  ut,
  esci = c("n", "y"),
  api_key = NULL,
  ...) {

  ut = as.character(ut)
  if (length(ut) > 100) {
    stop("maximum of 100 UTs can be submitted to API")
  }
  ut = paste(ut, collapse = ",")
  ut = gsub(" ", "", ut)


  if (is.logical(esci)) {
    esci = c("n", "y")[ esci + 1 ]
  } else {
    esci = tolower(esci)
    esci = match.arg(esci)
  }

  query = list()
  query$esci = esci
  query$UT = ut

  res = ws_build_request(
    api_key = api_key,
    api = "incites",
    endpoint = "DocumentLevelMetricsByUT",
    query = query)
  cr = ws_request_content(res, error = FALSE, ...)

  L = list(
    response = res,
    content = cr)
  class(L) = "ws_result"
  return(L)
}

#' @export
#' @rdname ws_incites_by_inst_id
#' @examples
#' ut = c("000352040700014","000353267900023",
#'  "000346982900018","000346342200004")
#' if (ws_have_incites_key()) {
#' result = ws_incites_invalid_ut(ut = ut,
#' flatten = TRUE)
#' docs = result$content$api$rval[[1]]
#' }
ws_incites_invalid_ut = function(
  ut,
  api_key = NULL,
  ...) {

  ut = as.character(ut)
  if (length(ut) > 100) {
    stop("maximum of 100 UTs can be submitted to API")
  }
  ut = paste(ut, collapse = ",")
  ut = gsub(" ", "", ut)


  query = list()
  query$UT = ut

  res = ws_build_request(
    api_key = api_key,
    api = "incites",
    endpoint = "CheckValidUTs",
    query = query)
  cr = ws_request_content(res, error = FALSE, ...)

  L = list(
    response = res,
    content = cr)
  class(L) = "ws_result"
  return(L)
}


#' @export
#' @rdname ws_incites_by_inst_id
#' @examples
#' if (ws_have_incites_key()) {
#' result = ws_incites_last_updated(flatten = TRUE)
#' result$content$api$rval[[1]]
#' }
ws_incites_last_updated = function(
  api_key = NULL,
  ...) {

  res = ws_build_request(
    api_key = api_key,
    api = "incites",
    endpoint = "InCitesLastUpdated")
  cr = ws_request_content(res, error = FALSE, ...)

  L = list(
    response = res,
    content = cr)
  class(L) = "ws_result"
  return(L)
}
