#' Download files from internet using url address and untar or unzip them.
#'
#' To avoid downloading existing files, the function verify if files exist locally,
#' and compare checksum value from previous download.
#'
#' @param urls A character string. Represents the url of file to be downloaded.
#'
#' @param destfile Character string giving the path where the downloaded file is saved.
#'                 Default will use the \code{modulePath} from the \code{sim} object,
#'                 if supplied with a module name, or a temporary location based on the
#'                 url of the file(s).
#'
#' @param sim A \code{simList} simulation object, generally produced by \code{SpaDES::simInit}.
#'
#' @param module A character string. Represents the names of the module to be loaded for the simulation.
#'
#' @param checkhash Logical. If \code{TRUE}, check if file exists locally and cross-check
#'        checksum value with value logged from previous download. When checksums match, no download occurs.
#'        When checksums don't match or file doesn't exist locally (i.e., first download), download occurs and
#'        checksum compiles. If \code{FALSE} (default), file is downloaded even if it is found locally.
#'
#' @param dbHash A character string. The path to the database file where checksum value of file is logged. If the
#'         database does not yet exist, one is created. Default is \code{"dbHash.sqlite"}.
#'
#' @param cascade Logical. If \code{TRUE}, file is untar and/or unzip. Default is \code{FALSE}.
#'
#' @param quick Logical. If \code{TRUE}, checksum is compiled using the combination of the filename and its size.
#'        If \code{FALSE} (default), cheksum is compiled using the object.
#'
#' @param quiet Logical. If \code{TRUE}, suppress status messages (if any), and the progress bar.
#'
#' @return Invoked for its side-effect of downloading files to the \code{destfile/} directory.
#'
#' @importFrom DBI dbConnect dbWriteTable dbReadTable dbExistsTable dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom SpaDES modulePath
#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom utils download.file
#' @docType methods
#' @author Melina Houle
#' @export
#' @rdname hashDownload
#' @examples
#' sim <- SpaDES::simInit(times = list(start = 0.0, end = 5.0),
#'                        objects = list(),
#'                        params = list(),
#'                        modules = list(),
#'                        paths = list(outputPath = tempdir()))
#' url <- "ftp://ccrp.tor.ec.gc.ca/pub/EC_data/AHCCD_daily/ZMekis_Vincent_2011.pdf"
#' hashDownload(urls = url, destfile = tempdir(), sim, module, checkhash = FALSE, cascade = FALSE)
#'
hashDownload <- function(urls, destfile, sim, module, checkhash = FALSE,
                         quick = FALSE, dbHash = "dbHash.sqlite", cascade = FALSE,
                         quiet = TRUE) {
  cwd <- getwd()
  if (!missing(destfile)) {
    setwd(destfile)
    on.exit(setwd(cwd))
  }
  if (!file.exists(destfile)) {
    dir.create(destfile, showWarnings = FALSE)
  }

  if (missing(destfile)) {
    if (!missing(sim)) {
      if (missing(module)) {
        stop("You must provide a module name, if you provide a simList.")
      }
      destfile <- file.path(modulePath(sim), module, "data")
    } else {
      destfile <- file.path(dirname(tempdir()),"data",file_path_sans_ext(basename(urls)))
    }
  }

  # Crosscheck checksum value fromtable where checksum values from previous download are compiled.
  if (checkhash) {
    # Connect to checksum db
    con <- dbConnect(SQLite(), dbHash)

    # Create checksum table if it doesn't exist
    if (!dbExistsTable(con, "checksum")) {
      dbWriteTable(con, "checksum", data.frame(Filename = character(),
                                               checksumFile = character(),
                                               checksumSize = character(),
                                               algorithm = character(),
                                               stringsAsFactors = FALSE),
                                               overwrite = TRUE, field.types = NULL)
      if (!quiet) message("hashDwd.txt has been created.")
    }

    hfile <- dbReadTable(con, "checksum")

    # Compile checksum on local file. Empty the first time the function is used.
    hashdata <- hList(basename(urls), destfile, quick)

    # List files already properly downloaded using logged checksum
    if (quick) {
      # Use checksum on filename and filesize
      dwdfile <- subset(hashdata, (hashdata$checksumSize %in% hfile$checksumSize) &
                          (hashdata$Filename %in% hfile$Filename))
    } else {
      # use checksum from file. Takes more time
      dwdfile <- subset(hashdata, (hashdata$checksumFile %in% hfile$checksumFile) &
                          (hashdata$Filename %in% hfile$Filename))
    }

    # Substract files already downloaded from list to download
    if (nrow(dwdfile) != 0) {
      file2dwd <- subset(urls, !basename(urls) %in% dwdfile$Filename)
      if (length(file2dwd) != 0) {
        needDownload <- TRUE
      } else {
        # List empty. All files downloaded
        needDownload <- FALSE
      }
    } else{
      # No files previously download. Need download.
      file2dwd <- urls
      needDownload <- TRUE
      if (!quiet)  message("Proceeding to download. checksum will be recorded")
    }

    #DOWNLOAD
    if (needDownload) {
      lapply(file2dwd, function(x) {
        download.file(x, file.path(destfile, basename(x)), method = "auto", mode = "wb", quiet)
      })
      # Logged checksum value
      hashdata < -hList(basename(file2dwd), destfile, quick)
      logHash(hashdata, dbHash)
      dbDisconnect(con)
    } else {
      if (!quiet) message("All files were previously downloaded. No download will occur.")
    }
  } else {
    # checksum = FALSE -> Download data only
    lapply(urls, function(x) {
      download.file(x, file.path(destfile, basename(x)), method = "auto", mode = "wb", quiet)
    })
  }

  file.list <- file.path(destfile, basename(urls))
  # Cascade = TRUE will untar / unzip all files
  if (cascade) {
    # Unzip
    invisible(lapply(file.list,  function(x) {
      if (file_ext(x) == "zip") {
        hashUnzip(x, checkhash = checkhash, quick = quick, dbHash = dbHash, destfile)
      }
    }))
    # Untar and unzip
    invisible(lapply(file.list, function(x) {
      if (file_ext(x) == "tar") {
        hashUntar(x, checkhash = checkhash, quick = quick, dbHash = dbHash, destfile)
      }
    }))
  }
  return(sim)
}
