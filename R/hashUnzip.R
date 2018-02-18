################################################################################
#' Extract files from a zip archive downloaded and log checksum value
#'
#' Unzip dataset previously downloaded from url, first checking if
#' unzip was previously performed (unzip file exists locally).
#'
#' @param zipfile A character vector. Contains path to zipfile.
#'
#' @inheritParams hashDownload
#'
#' @return Invoked for its side-effect of saving unzipped file in a subdirectory
#'         using the zipfile basename in the \file{destfile/} directory.
#'
#' @author Melina Houle
#' @docType methods
#' @importFrom DBI dbConnect dbReadTable dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom tools file_path_sans_ext
#' @importFrom utils unzip
#' @keywords internal
#' @export
#' @rdname hashUnzip
#'
#' @examples
#' u <-"http://ftp.geogratis.gc.ca/pub/nrcan_rncan/archive/vector/cli_itc_50k/land_use/L040J03.zip"
#' hashDownload(u, destfile = tempdir(), checkhash= FALSE, cascade = FALSE)
#' zip <- file.path(tempdir(), basename(u))
#' hashUnzip(zip, tempdir(), checkhash = FALSE)
hashUnzip <- function(zipfile, destfile, checkhash = TRUE, quick = FALSE,
                      dbHash = "dbHash.sqlite") {
  fn <- file_path_sans_ext(basename(zipfile))
  if (checkhash) {
    con <- dbConnect(SQLite(), dbHash)
    if (!dbExistsTable(con, "checksum")) {
      dbWriteTable(con, "checksum", .hdf(), overwrite = TRUE, field.types = NULL)
    }
    hfile <- dbReadTable(con, "checksum")

    dataPath <- file.path(destfile, fn)
    unzipped <- list.files(dataPath)
    if (!length(unzipped) == 0) {
      hashdata <- hList(basename(unzipped), dataPath, quick)
      # Check if files are properly unzipped using checksum.
      if (quick) {
        # List unzip files with mismatch checksum
        unlogged <- subset(hashdata, (!hashdata$checksumSize %in% hfile$checksumSize) &
                             (!hashdata$Filename %in% hfile$Filename))
        needUnzip <- if (nrow(unlogged) != 0) TRUE else FALSE
      } else {
        # List unzip files with mismatch checksum
        unlogged <- subset(hashdata, (!hashdata$checksumFile %in% hfile$checksumFile) &
                             (!hashdata$Filename %in% hfile$Filename))
        needUnzip <- if (nrow(unlogged) != 0) TRUE else FALSE
      }
    } else {
      # No file in unzip folder. Need to unzip.
      needUnzip <- TRUE
    }
    if (needUnzip) {
      # Unzip
      unzip(zipfile, exdir = dataPath)
      # Log checksum
      unzipped <- list.files(dataPath)
      checksum <- hList(unzipped, dataPath, quick)
      logHash(checksum, dbHash)
    }
    dbDisconnect(con)
  } else {
    #Checkhash = FALSE. Unzip only.
    unzip(zipfile, exdir = file.path(destfile, fn))
  }
}
