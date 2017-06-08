################################################################################
#' Extract files from a tar archive and log checksum value to avoid repeating event
#'
#' Prior to untar, the function check if untar was previously performed (untarred file exist
#' locally) and compare checksum value from previous event when present. Untar is performed
#' when file doesn't exists or checksums don't match.
#'
#' @param tarfile A character string. Represents path to tarfile.
#'
#' @param destfile A character string. Contains the name where untar file is saved.
#'
#' @param checkhash Logical. If \code{TRUE} (default), check if untarred file exists locally and compare
#'        checksum value with checksum logged from previous event. When checksums don't
#'        match or ubtar file doesn't exist , \code{untar} and \code{digest} are performed.
#'        If \code{FALSE}, only \code{untar} is performed.
#'
#' @param quick Logical. If \code{TRUE}, \code{digest} is performed using the combination of
#'        untarred filename and its size. If \code{FALSE} (default), \code{digest} is performed
#'        using the object.
#'
#' @param dbHash A character string. Represents path to SQLite database file where checksum value
#'        from \code{digest} is logged. If the database doesn't exist, one is created. Default is
#'        \code{"dbHash.sqlite"}.
#'
#' @return Untarred/unzipped \code{tarfile} in a subfolder under \code{destfile} using
#'         \code{basename{tarfile}} name.
#'
#' @importFrom utils untar
#' @importFrom tools file_path_sans_ext
#' @importFrom DBI dbConnect dbReadTable dbDisconnect
#' @importFrom RSQLite SQLite
#' @docType methods
#' @author Melina Houle
#' @export
#' @rdname hashUntar
#' @examples
#' urlpath <- "ftp://sidads.colorado.edu/pub/DATASETS/NOAA/G02171/Hudson_Bay/2006"
#' urlfile <-  "cis_SGRDRHB_20060904_pl_a.tar"
#' url <- file.path(urlpath, urlfile)
#' hashDownload(url, destfile = tempdir(), cascade = FALSE)
#' tar<- file.path(tempdir(), basename(url))
#' hashUntar(tar, tempdir(), checkhash= FALSE)
#'
hashUntar <-function(tarfile, destfile, checkhash = TRUE, quick = FALSE, dbHash = "dbHash.sqlite"){
  fx<- file_path_sans_ext(basename(tarfile))
  if(checkhash){

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
