################################################################################
#' Retrive file available to download.
#'
#' The function produce a character vector of file available to download. It uses a preset
#' \code{data.table} in which a list of relevant dataset name, their associate url and password
#' are stored. To retrive available file, the function derive URL, and username/password using
#' the \code{datasetName}.
#'
#' @param urlTble A \code{data.table} that stores available dataset name, url,
#'                password and filenames found within each dataset.
#'                \code{urltble} is provided within the package as urls object.
#'
#' @param datasetName Character string. Represent the dataset of interest for download.
#'                    \code{datasetName} allow to derived url and password from the \code{urltble}.
#'
#' @param dfile Character string representing filename of interest to download.
#'              When missing, all files from associated url given will be listed.
#'
#' @return Vector of url to download.
#'
#' @author Melina Houle
#' @docType methods
#' @keywords internal
#' @importFrom data.table setkey data.table
#' @importFrom plyr .
#' @importFrom RCurl getURL
#' @importFrom XML readHTMLTable
#' @keywords internal
#' @rdname listWebData
#'
#' @examples
#' dt <- data.table::data.table(
#'   dataset = c("NFDB"),
#'   url = c("http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/"),
#'   password = c(NA)
#' )
#' path2data <- listWebData(dt, datasetName = "NFDB", dfile = "NFDB_poly_20160712_metadata.pdf")
#'
listWebData <- function(urlTble, datasetName, dfile) {
  if (missing(urlTble)) {
    stop("You must provide a data.table that contain the link between url, datasetName and password.")
  }
  if (missing(datasetName)) {
    stop("You must provide dataset name to access url of interest.")
  }
  #if (missing(dfile)) dfile <- "all"
  setkey(urlTble, "dataset")
  password <- as.character(urlTble[.(datasetName)][, 'password', with = FALSE])
  if (password == "NA") password <- NA_character_
  url2data <- as.character(urlTble[.(datasetName)][, 'url', with = FALSE])

  # Split url into typeConn(ftp, http) and address.
  typeConn <- paste(unlist(strsplit(url2data, "//"))[1], "//", sep = "")
  address <- unlist(strsplit(url2data, "//"))[2]

  # Create list of file to download
  if (typeConn == "ftp://") {
      # No password for the ftp site
      if (is.na(password)) {
        file.list <- getURL(url2data, ftp.use.epsv = FALSE, dirlistonly = TRUE)
        file.list <- strsplit(file.list,"\r*\n")[[1]]
        file.list <- file.list[!file.list %in% c(".", "..")]
        file.list <- paste(url2data, file.list, sep = "")
    } else {
      # Password needed
      file.list <- getURL(url2data, userpwd = password, ftp.use.epsv = FALSE, dirlistonly = TRUE)
      file.list <- strsplit(file.list,"\r*\n")[[1]]
      file.list <- file.list[!file.list %in% c(".","..")]
      file.list <- paste(typeConn, password, "@", address, file.list, sep = "")
    }
  } else if (typeConn == "http://" || typeConn == "https://") {
      file.list <- readHTMLTable(url2data, skip.rows = 1:2)[[1]]$Name
      file.list <- paste(url2data, file.list[!is.na(file.list)], sep = "")
      file.list <- file.list[!file.list %in% c(".","..")]
  } else {
    stop("Unrecognized url type. Currently, only http://, https://, and ftp:// are supported.")
  }

  return(file.list)
}


#' Grab the current version of the urls from the git repository.
#'
#' Running this will get latest version of the urls, returned as a data.table.
#'
#' @return
#' Data.table with 4 columns, dataset, url, password (currently all NA), files, keyed by dataset, files
#'
#' @param wide Logical. If TRUE, the result is returned in long format.
#' @importFrom data.table data.table
webDatabases <- function(wide = TRUE) {
  source("https://raw.githubusercontent.com/PredictiveEcology/webDatabases/master/R/webDatabases.R", local = TRUE)
  if (wide) {
    urls <- urlsWide()[ , list( files = unlist( files )) , by = "dataset,url,password" ]
    setkey(urls, dataset, files)
  }
  message("Database retrieved from PredictiveEcology/webDatabases")
  urls
}
