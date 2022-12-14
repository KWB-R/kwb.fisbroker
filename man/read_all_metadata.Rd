% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_metadata.R
\name{read_all_metadata}
\alias{read_all_metadata}
\title{Read Metadata based on Overview on FIS Broker Datasets}
\usage{
read_all_metadata(
  overview = get_dataset_overview(),
  preserve_handle = TRUE,
  dbg = TRUE
)
}
\arguments{
\item{overview}{overview tibble as retrieved by \code{\link{get_dataset_overview}},
(default: \code{\link{get_dataset_overview}})}

\item{preserve_handle}{logical. If TRUE (the default is FALSE), the Curl
handle is created in advance and reused for all \code{\link[httr]{GET}}
requests.}

\item{dbg}{whether or not to show debug messages (default: TRUE)}
}
\value{
tibble with all metdata information
}
\description{
Read Metadata based on Overview on FIS Broker Datasets
}
\examples{
\dontrun{
overview <- kwb.fisbroker::get_dataset_overview()
overview_atom <- overview[overview$type == "ATOM",]
metadata_atom <- kwb.fisbroker::read_all_metadata(overview_atom)
overview_wfs <- overview[overview$type == "WFS",]
metadata_wfs <- kwb.fisbroker::read_all_metadata(overview_wfs)
overview_wms <- overview[overview$type == "WMS",]
metadata_wms <- kwb.fisbroker::read_all_metadata(overview_wms)
}
}
