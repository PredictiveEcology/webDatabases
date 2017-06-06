################################################################################
#' Extract files from a zip archive downloaded and log checksum value to avoid repeating action.
#'
#' This function unzip dataset previously downloaded from url. Prior to unzip, the function check if
#' unzip was previously performed (unzip file exists locally).
#'
#' @param zipfile A character vector. Contain path to a zipfile.
#' @param destfile A character string. Represents the path where unzip file is saved.
#' @param checkhash A logical argument. If TRUE, check if unzip file is found locally and cross-check its checksum value
#'        with value logged in dbHash from previous unzip event. When checksums match, a message raises indicating the file
#'        is already properly unzip. When checksums doesn't match or unzip file doesn't exist locally, unzip occurs and
#'        checksum compiles. If FALSE, unzip occurs even if the files exist locally. Default is FALSE.
#' @param quick A logical argument. If TRUE, checksum is compiled using the combination of the filename and its size.
#'        If FALSE, cheksum is compiled using the object. Default is FALSE.
#' @param dbHash A character string. The path to the database file where file checksum value is logged. If the named
#'        database does not yet exist, one is created. Default is "dbHash.sqlite".
#' @return Unzipped file is saved in a subfolder using the zipfile basename in the destfile folder.
#' @importFrom utils unzip
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
#' url<-"ftp://ccrp.tor.ec.gc.ca/pub/EC_data/AHCCD_daily/Homog_daily_mean_temp_v2016.zip"
#' algo = "xxhash64"
#' destfile <- tempdir()
#' hashDownload(url, destfile, sim, module, cascade = FALSE)
#' zip <- file.path(destfile, basename(url))
#' hashUnzip(zip, destfile, checkhash = FALSE,  quick = FALSE, dbHash = "dbHash.sqlite")
hashUnzip <-function(zipfile, destfile, checkhash = TRUE, quick = TRUE, dbHash){
  fn<- file_path_sans_ext(basename(zipfile))
  if(checkhash){
    if(missing(dbHash)) {
      stop("You must provide a name to database where checksum are or will be stored")
    }
    con = dbConnect(SQLite(), dbHash)
    if (!dbExistsTable(con, "checksum")){
      dbWriteTable(con, "checksum", data.frame(Filename= character(),
                                               checksumFile= character(),
                                               checksumSize= character(),
                                               algorithm = character(),
                                               stringsAsFactors=FALSE),
                   overwrite = TRUE, field.types = NULL)
    }
    hfile <-dbReadTable(con, "checksum")

    dataPath <- file.path(destfile,fn)
    unzipped<-list.files(dataPath)
    if(!length(unzipped)==0) {
      hashdata<-hList(basename(unzipped), dataPath, quick)
      # Check if files are properly unzipped using checksum.
      if (quick) {
        # List unzip files with mismatch checksum
        unlogged<-subset(hashdata, (!hashdata$checksumSize %in% hfile$checksumSize) &
                           (!hashdata$Filename %in% hfile$Filename))
        if(nrow(unlogged)!=0)  {
          needUnzip <- TRUE
        }else{
          needUnzip <- FALSE
        }
      }else{
        # List unzip files with mismatch checksum
        unlogged<-subset(hashdata, (!hashdata$checksumFile %in% hfile$checksumFile) &
                           (!hashdata$Filename %in% hfile$Filename))
        if(nrow(unlogged)!=0)  {
          needUnzip <- TRUE
        }else{
          needUnzip <- FALSE
        }
      }
    }else{
      # No file in unzip folder. Need to unzip.
      needUnzip <- TRUE
    }
    if(needUnzip){
      # Unzip
      unzip(zipfile, exdir=dataPath)
      # Log checksum
      unzipped<-list.files(dataPath)
      checksum<-hList(unzipped, dataPath, quick)
      logHash(checksum, dbHash)
    }
    dbDisconnect(con)
  }else{
    #Checkhash = FALSE. Unzip only.
    unzip(zipfile, exdir=file.path(destfile,fn))
  }
}
