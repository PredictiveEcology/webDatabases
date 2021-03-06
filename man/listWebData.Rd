% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/listWebData.R
\docType{methods}
\name{listWebData}
\alias{listWebData}
\title{Retrive file available to download.}
\usage{
listWebData(urlTbl, datasetName, dfile)
}
\arguments{
\item{urlTbl}{A \code{data.table} that stores available dataset name, url,
password and filenames found within each dataset.
\code{urlTbl} is provided within the package as urls object.}

\item{datasetName}{Character string. Represent the dataset of interest for download.
\code{datasetName} allow to derived url and password from the \code{urlTbl}.}

\item{dfile}{Character string representing filename of interest to download.
When missing, all files from associated url given will be listed.}
}
\value{
Vector of url to download.
}
\description{
Produce a character vector of files available to download.
It uses a preset \code{data.table} in which a list of relevant dataset name,
their associate url and password are stored.
To retrive available file, the function derive URL, and username/password using
the \code{datasetName}.
}
\examples{
\dontrun{
dt <- data.table::data.table(
  dataset = c("NFDB"),
  url = c("http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/"),
  password = c(NA)
)
path2data <- listWebData(dt, datasetName = "NFDB", dfile = "NFDB_poly_20160712_metadata.pdf")
}
}
\author{
Melina Houle
}
\keyword{internal}
