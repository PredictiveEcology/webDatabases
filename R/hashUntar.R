################################################################################
#' Extract files from a tar archive downloaded and log checksum value
#'
#' untar and unzip files, first checking if untar was previously performed
#' (untarred/unzipped file exist locally).
#'
#' @param tarfile A character vector. Contains tarfile path.
#'
#' @param destfile A character string. Represents the path where untar file is saved.
#'
#' @param checkhash Logical. If \code{TRUE}, check if untarred/unzipped file exists
#'                  locally and cross-checks the checksum value with logged checksum
#'                  from previous untar event.
#'                  When checksums don't match or file doesn't exist locally,
#'                  untar occurs and checksum is computed.
#'                  If \code{FALSE} (default), file is untarred.
#'
#' @param dbHash Character string. The path to the database file where checksum value of file is logged.
#'               If the database does not yet exist, one is created. Default is \code{"dbHash.sqlite"}.
#'
#' @param quick Logical. If \code{TRUE}, checksum is computed using the combination
#'              of the filename and file size. If \code{FALSE} (default), cheksum
#'              is computed using the object.
#'
#' @return Used for its side-effect: the untarred/unzipped file is saved in a
#'         subdirectory using the tarfile basename in the \file{destfile/} directory.
#'
#' @author Melina Houle
#' @docType methods
#' @export
#' @importFrom utils untar
#' @importFrom tools file_path_sans_ext
#' @importFrom DBI dbConnect dbReadTable dbDisconnect
#' @importFrom RSQLite SQLite
#'
#' @examples
#' \dontrun{
#' sim <- SpaDES::simInit(times = list(start = 0.0, end = 5.0),
#'                        objects = list(),
#'                        params = list(),
#'                        modules = list(),
#'                        paths = list(outputPath = tempdir()))
#' url <- "ftp://knn4ftp:knn4ftp@tree.nfis.org/kNN-LandCover.tar"
#' destfile <- tempdir()
#' hashDownload(url, destfile, sim, module, destfile, checkhash = FALSE, cascade = FALSE)
#' tar<- file.path(destfile, basename(url))
#' hashUntar(tar, destfile, checkhash = FALSE, dbHash = "dbHash.sqlite", quick = FALSE)
#' }
#'
hashUntar <- function(tarfile, destfile, checkhash = FALSE, dbHash = "dbHash.sqlite", quick = FALSE) {
  fx <- file_path_sans_ext(basename(tarfile))
  if (checkhash) {
    if (missing(dbHash)) {
      stop("You must provide a name to database where checksum are or will be stored.")
    }
    # Crosscheck with previous download
    con <- dbConnect(SQLite(), dbHash)
    if (!dbExistsTable(con, "checksum")) {
      dbWriteTable(con, "checksum", data.frame(Filename = character(),
                                               checksumFile = character(),
                                               checksumSize = character(),
                                               algorithm = character(),
                                               stringsAsFactors = FALSE),
                   overwrite = TRUE, field.types = NULL)
    }

    hfile <- dbReadTable(con, "checksum")

    # output directory exists
    fileNames <- list.files(file.path(destfile, fx))
    dataPath <- file.path(destfile, fx)

    if (!length(fileNames) == 0) {
      hashdata <- hList(basename(fileNames), dataPath, quick)
      # Files exist. Check if they were properly untar using checksum.
      if (quick) {
        # List untar files with missing checksum
        unlogged <- subset(hashdata, (!hashdata$checksumSize %in% hfile$checksumSize) &
                             (!hashdata$Filename %in% hfile$Filename))
        needUntar <- if (nrow(unlogged) != 0) TRUE else FALSE
      } else {
        # List untar files with missing checksum
        unlogged <- subset(hashdata, (!hashdata$checksumFile %in% hfile$checksumFile) &
                             (!hashdata$Filename %in% hfile$Filename))
        needUntar <- if (nrow(unlogged) != 0) TRUE else FALSE
      }
    } else {
      # No file exists in untar folder. Need to untar.
      needUntar <- TRUE
    }

    if (needUntar) {
      # Untar
      untar(tarfile, exdir = file.path(destfile, fx))

      # Log checksum
      fileNames <- list.files(file.path(destfile, fx))
      dataPath <- file.path(destfile, fx)
      checksum <- hList(fileNames, dataPath, quick)
      logHash(checksum, dbHash)

      # Unzip and checksum
      file.list <- unlist(lapply(fileNames, function(x) {file.path(dataPath, x)}))
      lapply(file.list, function(i) if (file_ext(i) == "zip") {
        hashUnzip(i, dataPath, checkhash = checkhash, quick = quick, dbHash = dbHash)
      })
    }
    dbDisconnect(con)
  } else {
    #Checkhash = FALSE. Untar and unzip only. No cheksum.
    untar(tarfile, exdir = file.path(destfile, fx))

    # Unzip
    fileNames <- list.files(file.path(destfile, fx))
    dataPath <- file.path(destfile, fx)
    file.list <- unlist(lapply(fileNames, function(x) {file.path(dataPath, x)}))
    lapply(file.list, function(i) if (file_ext(i) == "zip") {
      hashUnzip(i, dataPath, checkhash = checkhash, quick = quick, dbHash = file.path(destfile, dbHash))})
  }
}
