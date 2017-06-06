################################################################################
#' Extract files from a tar archive downloaded and log checksum value to avoid repeating action.
#'
#' This function untar and unzip files. Prior to untar, the function check if untar
#' was previously performed (untarred/unzipped file exist locally).
#'
#' @param tarfile A character vector.  Contains tarfile path.
#' @param destfile A character string. Represents the path where untar file is saved.
#' @param checkhash A logical argument. If TRUE, check if untarred/unzipped file exists locally
#'                  and cross-check checksum value with logged checksum from previous untar event.
#'                  When checksums don't match or file doesn't exist locally, untar occurs and
#'                  checksum compiles. If FALSE, file is untarred Default is FALSE.
#' @param dbHash A character string. The path to the database file where checksum value of file is logged.
#'               If the database does not yet exist, one is created. Default is "dbHash.sqlite".
#' @param quick A logical argument. If TRUE, checksum is compiled using the combination of the filename and
#'              its size. If FALSE, cheksum is compiled using the object. Default is FALSE.
#' @return Untarred/unzipped file is saved in a subfolder using the tarfile basename in the destfile folder.
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
#' hashUntar(tar, destfile, checkhash = FALSE, dbHash = "dbHash.sqlite", quick = FALSE)
hashUntar <-function(tarfile, destfile, checkhash = FALSE, dbHash = "dbHash.sqlite", quick=FALSE){
  fx<- file_path_sans_ext(basename(tarfile))
  if(checkhash){
    if(missing(dbHash)) {
      stop("You must provide a name to database where checksum are or will be stored")
    }
    # Crosscheck with previous download
    con = dbConnect(SQLite(), dbHash)
    if (!dbExistsTable(con, "checksum")){
      dbWriteTable(con, "checksum", data.frame(Filename= character(),
                                               checksumFile= character(),
                                               checksumSize= character(),
                                               algorithm = character(),
                                               stringsAsFactors=FALSE),
                   overwrite = TRUE, field.types = NULL)
    }

    hfile<-hfile <-dbReadTable(con, "checksum")

    # output directory exists
    fileNames<-list.files(file.path(destfile,fx))
    dataPath <- file.path(destfile,fx)

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
      untar(tarfile, exdir = file.path(destfile, fx))

      # Log checksum
      fileNames<-list.files(file.path(destfile,fx))
      dataPath <- file.path(destfile,fx)
      checksum<-hList(fileNames, dataPath, quick)
      logHash(checksum, dbHash)

      # Unzip and checksum
      file.list <- unlist(lapply(fileNames, function(x){file.path(dataPath, x)}))
      lapply(file.list, function(i) if (file_ext(i) == "zip") {
        hashUnzip(i, dataPath, checkhash = checkhash, quick = quick, dbHash = dbHash)})
    }
    dbDisconnect(con)
  }else{
    #Checkhash = FALSE. Untar and unzip only. No cheksum.
    untar(tarfile, exdir = file.path(destfile, fx))

    # Unzip
    fileNames<-list.files(file.path(destfile,fx))
    dataPath <- file.path(destfile,fx)
    file.list <- unlist(lapply(fileNames, function(x){file.path(dataPath, x)}))
    lapply(file.list, function(i) if (file_ext(i) == "zip") {
      hashUnzip(i, dataPath, checkhash = checkhash, quick = quick, dbHash = file.path(destfile,dbHash))})
  }
}



