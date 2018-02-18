################################################################################
#' Download and store hash value from downloaded file
#'
#' To avoid redownloading existing files, verify if the file exists locally.
#' If the file exists, compare the checksum value to that logged by original download.
#'
#' @param urls A character string. Represents the url of file to be downloaded.
#'
#' @param destfile A character string. Represents the name where resulting file is saved.
#'
#' @param checkhash Logical. If \code{TRUE} (default), check if file exists locally
#'                  and cross-check the file's checksum value with that logged
#'                  from a previous event, skippng redownload if checksum matches.
#'                  When checksums don't match or file doesn't exist locally,
#'                  the file is downloaded and its checksum computed.
#'                  If \code{FALSE}, the function is executed even if the file
#'                  is found locally.
#'
#' @param quick Logical. If \code{TRUE}, checksum is compiled using the combination
#'              of the filename and its size.
#'              If \code{FALSE} (default), cheksum is compiled using the object.
#'
#' @param dbHash Character string. The path to the database file where checksum
#'               value of file is logged.
#'               If the database does not yet exist, one is created. Default is \code{"dbHash.sqlite"}.
#'
#' @param cascade Logical. If \code{FALSE}, file is untar and/or unzip. Default is \code{FALSE}.
#'
#' @param quiet Logical. If \code{TRUE}, suppress status messages (if any), and the progress bar.
#'
#' @return Invoked for its side-effect of downloading files to the \code{destfile/} directory.
#'
#' @author Melina Houle
#' @docType methods
#' @importFrom DBI dbConnect dbWriteTable dbReadTable dbExistsTable dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom utils download.file
#' @rdname hashDownload
#' @keywords internal
#' @export
#'
#' @examples
#' u <-"http://ftp.geogratis.gc.ca/pub/nrcan_rncan/archive/vector/cli_itc_50k/land_use/L040J03.zip"
#' hashDownload(urls = u, destfile = tempdir(), checkhash = FALSE, cascade = FALSE)
hashDownload <- function(urls, destfile, checkhash = TRUE, quick = FALSE,
                         dbHash = "dbHash.sqlite", cascade = FALSE, quiet = TRUE) {
  cwd <- getwd()
  if (!missing(destfile)) {
    setwd(destfile)
    on.exit(setwd(cwd))
  }
  if (!file.exists(destfile)) {
    dir.create(destfile, showWarnings = FALSE)
  }

  if (missing(destfile)) {
    stop("You must provide an output path to store downloaded file.")
  }

  # Crosscheck checksum value fromtable where checksum values from previous download are compiled.
  if (checkhash) {
    # Connect to checksum db
    con <- dbConnect(SQLite(), dbHash)

    # Create checksum table if it doesn't exist
    if (!dbExistsTable(con, "checksum")) {
      dbWriteTable(con, "checksum", .hdf(), overwrite = TRUE, field.types = NULL)
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
    } else {
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
      hashdata <- hList(basename(file2dwd), destfile, quick)
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
}
