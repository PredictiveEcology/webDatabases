################################################################################
#' Extract files from a tar archive and log checksum value to avoid repeating event
#'
#' Prior to untar, the function check if untar was previously performed (untarred
#' file exist locally) and compare checksum value from previous event when present.
#' Untar is performed when file doesn't exists or checksums don't match.
#'
#' @inheritParams hashDownload
#'
#' @param tarfile A character string. Represents path to tarfile.
#'
#' @return Untarred/unzipped \code{tarfile} in a subfolder under \code{destfile} using
#'         \code{basename{tarfile}} name.
#'
#' @author Melina Houle
#' @docType methods
#' @importFrom DBI dbConnect dbReadTable dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom tools file_path_sans_ext
#' @importFrom utils untar
#' @keywords internal
#' @rdname hashUntar
#' @export
#'
#' @examples
#'   urlpath <- "ftp://sidads.colorado.edu/pub/DATASETS/NOAA/G02171/Hudson_Bay/2006"
#'   urlfile <-  "cis_SGRDRHB_20060904_pl_a.tar"
#'   url <- file.path(urlpath, urlfile)
#'   hashDownload(url, destfile = tempdir(), cascade = FALSE)
#'   tar<- file.path(tempdir(), basename(url))
#'   hashUntar(tar, tempdir(), checkhash= FALSE)
hashUntar <- function(tarfile, destfile, checkhash = TRUE, quick = FALSE, dbHash = "dbHash.sqlite") {
  fx <- file_path_sans_ext(basename(tarfile))
  if (checkhash) {
    # Crosscheck with previous download
    con <- dbConnect(SQLite(), dbHash)
    if (!dbExistsTable(con, "checksum")) {
      dbWriteTable(con, "checksum", .hdf(), overwrite = TRUE, field.types = NULL)
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
      file.list <- unlist(lapply(fileNames, function(x) file.path(dataPath, x)))
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
