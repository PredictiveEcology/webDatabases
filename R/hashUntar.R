################################################################################
#' Extract files from a tar archive downloaded
#'
#' This function untar and unzip dataset previously downloaded from url. Prior to untar, the function can check if untar
#' was previously performed (untar/unzip files exist locally).
#'
#' @param tarfile A character vector containing path to tarfile.
#' @param checkhash A logical argument. If TRUE, check if untar/unzip files are found locally and cross-check their checksum value
#'        with value logged in dbHash from previous untar/unzip event. When checksums match, a message raises indicating the files
#'        are already properly untar/unzip. When checksums don't match or untar/unzip files don't exist locally, untar/unzip occurs and
#'        checksum compiles. If FALSE, untar/unzip occurs even if the files exist locally. Default is FALSE.
#' @param dbHash A character string. The path to the database file where checksum values are logged If the named
#'        database does not yet exist, one is created. Default is "dbHash.sqlite".
#' @param quick A logical argument. If TRUE, checksum is compiled using the combination of the filename and its size.
#'        If FALSE, cheksum is compiled using the object. Default is FALSE.
#' @param dir A character string representing path to output directory.
#'
#' @importFrom utils untar
#' @importFrom tools file_path_sans_ext
#' @importFrom DBI dbConnect dbReadTable dbDisconnect
#' @importFrom RSQLite SQLite
#' @export
#' @docType methods
#'
#' @author Melina Houle
#' @examples
#' library(SpaDES)
#' sim <- SpaDES::simInit(times = list(start=0.0, end=5.0),
#'                        objects=list(),
#'                        params = list(),
#'                        modules = list(),
#'                        paths = list(outputPath=tempdir()))
#' url <- "ftp://knn4ftp:knn4ftp@tree.nfis.org/kNN-LandCover.tar"
#' destfile <- tempdir()
#' hashDownload(url, destfile, sim, module, destfile, checkhash = FALSE, cascade = FALSE)
#' tar<- file.path(destfile, basename(url))
#' hashUntar(tar, checkhash = FALSE, dbHash = "dbHash.sqlite", quick = FALSE , destfile)
hashUntar <-function(tarfile, checkhash = FALSE, dbHash = "dbHash.sqlite", quick=FALSE, dir){
  subdir <- dirname(tarfile)
  fx<- file_path_sans_ext(basename(tarfile))
  if(checkhash){
    # Crosscheck with previous download
    con = dbConnect(SQLite(), file.path(dir,dbHash))
    hfile<-hfile <-dbReadTable(con, "checksum")

    # output directory exists
    fileNames<-list.files(file.path(subdir,fx))
    dataPath <- file.path(subdir,fx)

    if(!length(fileNames)==0) {
      hashdata<-hList(basename(fileNames), dataPath, quick)
      # Files exist. Check if they were properly untar using checksum.
      if(quick){
        # List untar files with missing checksum
        unlogged<-subset(hashdata, (!hashdata$checksumSize %in% hfile$checksumSize) &
                           (!hashdata$Filename %in% hfile$Filename))
        if(nrow(unlogged)!=0)  {
          needUntar <- TRUE
        }else{
          needUntar <- FALSE
        }
      }else{
        # List untar files with missing checksum
        unlogged<-subset(hashdata, (!hashdata$checksumFile %in% hfile$checksumFile) &
                           (!hashdata$Filename %in% hfile$Filename))
        if(nrow(unlogged)!=0)  {
          needUntar <- TRUE
        }else{
          needUntar <- FALSE
        }
      }
    }else{
      # No file exists in untar folder. Need to untar.
      needUntar <- TRUE
    }

    if(needUntar){
      # Untar
      untar(file.path(subdir,basename(tarfile)), exdir = file.path(subdir, fx))

      # Log checksum
      fileNames<-list.files(file.path(subdir,fx))
      dataPath <- file.path(subdir,fx)
      checksum<-hList(fileNames, dataPath, quick)
      logHash(checksum, dbHash)

      # Unzip and checksum
      file.list <- unlist(lapply(fileNames, function(x){file.path(dataPath, x)}))
      lapply(file.list, function(i) if (file_ext(i) == "zip") {
        hashUnzip(i, checkhash = checkhash, quick = quick, dbHash = dbHash, dataPath)})
    }
    dbDisconnect(con)
  }else{
    #Checkhash = FALSE. Untar and unzip only. No cheksum.
    untar(file.path(subdir,basename(tarfile)), exdir = file.path(subdir, fx))

    # Unzip
    fileNames<-list.files(file.path(subdir,fx))
    dataPath <- file.path(subdir,fx)
    file.list <- unlist(lapply(fileNames, function(x){file.path(dataPath, x)}))
    lapply(file.list, function(i) if (file_ext(i) == "zip") {
      hashUnzip(i, checkhash = checkhash, quick = quick, dbHash = dbHash, dataPath)})
  }
}



