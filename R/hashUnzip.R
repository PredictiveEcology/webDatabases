################################################################################
#' Extract files from a zip archive and log checksum value to avoid repeating event
#'
#' Prior to unzip, the function check if untar was previously performed (unzipped file exist
#' locally) and compare checksum value from previous event when present. Unzip is performed
#' when file doesn't exists or checksums don't match.
#'
#' @param zipfile A character vector. Contains path to zipfile.
#'
#' @param destfile A character string. Represents the name where unzip file is saved.
#'
#' @param checkhash Logical. If \code{TRUE}, check if untzipped file exists locally and compare
#'        checksum value with checksum logged from previous event. When checksums don't
#'        match or unzip file doesn't exist, \code{unzip} and \code{digest} are performed.
#'        If \code{FALSE} (default), only \code{unzip} is performed.
#'
#' @param quick Logical. If \code{TRUE}, \code{digest} is performed using the combination of
#'        unzipped filename and its size. If \code{FALSE} (default), \code{digest} is performed
#'        using the object.
#'
#' @param dbHash A character string. Represents path to SQLite database file where checksum value
#'        from \code{digest} is logged. If the database doesn't exist, one is created. Default is
#'        \code{"dbHash.sqlite"}.
#'
#' @return Unzipped \code{zipfile} in a subfolder under \code{destfile} using
#'          \code{basename{tarfile}}name.

#' @importFrom utils unzip
#' @importFrom tools file_path_sans_ext
#' @importFrom DBI dbConnect dbReadTable dbDisconnect
#' @importFrom RSQLite SQLite
#' @docType methods
#' @author Melina Houle
#' @export
#' @rdname hashUnzip
#' @examples
#' url<-"http://ftp.geogratis.gc.ca/pub/nrcan_rncan/archive/vector/cli_itc_50k/land_use/L040J03.zip"
#' hashDownload(url, destfile = tempdir(), checkhash= FALSE, cascade = FALSE)
#' zip <- file.path(tempdir(), basename(url))
#' hashUnzip(zip, tempdir(), checkhash= FALSE)
hashUnzip <-function(zipfile, destfile, checkhash = TRUE, quick = FALSE, dbHash = "dbHash.sqlite"){
  fn<- file_path_sans_ext(basename(zipfile))
  if(checkhash){

    con = dbConnect(SQLite(), dbHash)
    if (!dbExistsTable(con, "checksum")){
      dbWriteTable(con, "checksum", data.frame(Filename= character(),
                                               checksumFile= character(),
                                               checksumSize= character(),
                                               algorithm = character(),
                                               stringsAsFactors = FALSE),
                   overwrite = TRUE, field.types = NULL)
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
