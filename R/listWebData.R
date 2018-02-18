if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("dataset", "files"))
}

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
#' \dontrun{
#' dt <- data.table::data.table(
#'   dataset = c("NFDB"),
#'   url = c("http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/"),
#'   password = c(NA)
#' )
#' path2data <- listWebData(dt, datasetName = "NFDB", dfile = "NFDB_poly_20160712_metadata.pdf")
#' }
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


#' Grab the current version of the urls either locally or from the git repository
#'
#' Running this will get latest version of the urls, returned as a data.table.
#'
#' @return
#' Data.table with 4 \code{columns}, \code{dataset}, \code{url}, \code{password},
#' and \code{files}, keyed by \code{dataset},
#'  \code{files}
#'
#' @param dbUrl Character string where to look for web database. Defaults to the web database
#'              See details.
#'
#' @param local Logical. If \code{FALSE} the function gets the latest webDatabase table from the online
#'              repository. This will allow for the user to be always up to date, but it also slower than
#'              \code{TRUE}. If \code{TRUE}, then this will take the version of the webDatabase that
#'              existed when the user installed the package.
#'
#' @param wide Logical. If \code{TRUE}, returns the wide form of database. Default \code{FALSE}
#'
#' @details
#' The current webDatabase is located at
#' \href{https://github.com/PredictiveEcology/webDatabases/blob/master/R/webDatabases.R}{
#' Web Database}
#' @importFrom data.table data.table
#' @importFrom RCurl url.exists
#' @export
#' @examples
#' data <- webDatabases()
#'
#' # Which datasets are available?
#' unique(data$dataset)
#'
#' # pick out KNN
#' data[dataset=="KNN"]
webDatabases <- function(dbUrl = NULL,
                         local = FALSE, wide = FALSE) {

  if (!local) {
    if (!RCurl::url.exists(dbUrl)) {
      local <- FALSE
    }
  }
  if (!local) {
    if (is.null(dbUrl))
      dbUrl <- "https://raw.githubusercontent.com/PredictiveEcology/webDatabases/master/R/webDatabases.R"
    source(dbUrl, local = TRUE)
    message("Database retrieved from PredictiveEcology/webDatabases")
  } else {
    message("Database retrieved locally because ", dbUrl, " is not reachable")
  }

  urls <- urlsWide()
  if (!wide) {
    urls <- urls[ , list( files = unlist( files )) , by = "dataset,url,password" ]
    setkey(urls, dataset, files)
  }
  urls
}
