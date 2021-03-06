% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ws_api_url.R
\name{ws_api_url}
\alias{ws_api_url}
\alias{ws_incites_url}
\alias{ws_endnote_url}
\alias{ws_tipms_url}
\alias{ws_wos_url}
\alias{ws_woslite_url}
\title{Web of Science API URL}
\usage{
ws_api_url(api = NULL)

ws_incites_url()

ws_endnote_url()

ws_tipms_url()

ws_wos_url()

ws_woslite_url()
}
\arguments{
\item{api}{an endpoint for the API, see
\url{https://developer.clarivate.com/apis}}
}
\value{
A character path of the URL
}
\description{
Web of Science API URL
}
\examples{
ws_api_url()
ws_incites_url()
ws_api_url(api = "incites")
ws_endnote_url()
ws_tipms_url()
ws_wos_url()
ws_woslite_url()
}
