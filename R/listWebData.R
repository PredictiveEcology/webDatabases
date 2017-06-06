################################################################################
#' Creates a vector of url to download based on datasetName and filename.
#'
#' Browse http and ftp site of interest using dataset name and store available dataset in a list.
#' To list the data using dataset name, a \code{data.table} containing \code{datasetName},
#' url of interest and password need to be previously created.
#'
#' @param urlTble A \code{data.table} that stores available dataset name, url,
#'                password and filenames found within each dataset.
#'                \code{urltble} is provided within the package as urls object.
#'
#' @param datasetName Character string. Represent the dataset of interest for download.
#'                    \code{datasetName} allow to derived url and password from the \code{urltble}.
#'
#' @param dfile Character string representing filename of interest to download.
#'              When missing, all files from associated url given will be listed.
#'
#' @return Vector of url to download.
#'
#' @author Melina Houle
#' @docType methods
#' @export
#' @importFrom RCurl getURL
#' @importFrom XML readHTMLTable
#' @importFrom data.table setkey
#' @importFrom plyr .
#' @rdname listWebData
#'
#' @examples
#' dt <- data.table::data.table(
#'   dataset = c("NFDB"),
#'   url = c("http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/"),
#'   password = c(NA)
#' )
#' path2data <- listWebData(dt, datasetName = "NFDB", dfile = "NFDB_poly_20160712_metadata.pdf")
#'
listWebData <- function(urlTble, datasetName, dfile) {
  if (missing(urlTble)) {
    stop("You must provide a data.table that contain the link between url, datasetName and password.")
  }
  if (missing(datasetName)) {
    stop("You must provide dataset name to access url of interest.")
  }
  if (missing(dfile)) dfile <- "all"
  setkey(urlTble, "dataset")
  password <- as.character(urlTble[.(datasetName)][, 'password', with = FALSE])
  url2data <- as.character(urlTble[.(datasetName)][, 'url', with = FALSE])

  # Split url into typeConn(ftp, http) and address.
  typeConn <- paste(unlist(strsplit(url2data, "//"))[1], "//", sep = "")

  # Create list of file to download
  # FTP connection
  if (typeConn == "ftp://") {
    if (isTRUE(dfile == "all")) {
      # all files to download. No password for the ftp site
      if (is.na(password)) {
        file.list <- getURL(url2data, ftp.use.epsv = FALSE, dirlistonly = TRUE)
      }
      file.list <- strsplit(file.list,"\r*\n")[[1]]
      file.list <- file.list[!file.list %in% c(".", "..")]
      file.list <- paste(url2data, file.list, sep = "")
    } else {
      # all files to download. Password needed for the ftp site
      address <- unlist(strsplit(url2data, "//"))[2]
      file.list <- getURL(url2data, userpwd = password, ftp.use.epsv = FALSE, dirlistonly = TRUE)
      file.list <- strsplit(file.list,"\r*\n")[[1]]
      file.list <- file.list[!file.list %in% c(".","..")]
      file.list <- paste(typeConn, password, "@", address, file.list, sep = "")
    }
  } else {
    # Specific files to download on the ftp site no password required
    if (is.na(password)) {
      file.list <- paste(url2data, dfile, sep = "")
    } else {
      # Specific files to download on the ftp site, password is required
      file.list <- paste(typeConn, password, "@", address, dfile, sep = "")
    }
  }

  ## HTTP connection
  if (typeConn == "http://" | typeConn == "https://") {
    if (isTRUE(dfile == "all")) {
      file.list <- readHTMLTable(url2data, skip.rows = 1:2)[[1]]$Name
      file.list <- paste(url2data, file.list[!is.na(file.list)], sep = "")
      file.list <- file.list[!file.list %in% c(".","..")]
    } else {
      file.list <- paste(url2data, dfile, sep = "")
    }
  }

  return(file.list)
}

################################################################################
#' Table of dataset accessible using url
#'
#' An R object that stores dataset with their respective url and username/password. This table makes the link between
#' dataset name, url and username/password. By only poviding the dataset name, listWebdata and dwdData functions extract url
#' username/password to access, extract and download data.
#'
#' @rdname webdataset
#' @return data.table object containing dataset available for download.
#'
#' @importFrom data.table data.table
#' @export
#' @docType methods
#'
#' @author Melina Houle
#' @examples
#' library(data.table)
#' dt<- data.table(
#'   dataset = c("NFDB"),
#'   url = "http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/",
#'   password = NA,
#'   files = "NFDB_poly.zip"
#' )
#'
urls <- data.table(
  dataset = c("AHCCD_daily",
              "NFDB",
              "KNN",
              "BIOSIM",
              "LCC2005",
              "NALCMS2005",
              "NALCMS2010",
              "EOSD2000"),
  url = c("ftp://ccrp.tor.ec.gc.ca/pub/EC_data/AHCCD_daily/",
          "http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/",
          "ftp://tree.nfis.org/",
          "ftp://ftp.nofc.cfs.nrcan.gc.ca/uploads/MPB/",
          "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/",
          "http://www.cec.org/sites/default/files/Atlas/Files/Land_Cover_2005/",
          "http://www.cec.org/sites/default/files/Atlas/Files/Land_Cover_2010/",
          "http://tree.pfc.forestry.ca/"),
  password = c(NA,
               NA,
               "knn4ftp:knn4ftp",
               NA,
               NA,
               NA,
               NA,
               NA),
  files = list(
    c("Adj_Daily_Rain_v2016.zip","Adj_Daily_Snow_v2016.zip","Adj_Daily_TotalP_v2016.zip",
      "Adj_Precipitation_Documentation_v2016_Daily.doc","Adj_Precipitation_Stations_v2016.xls",
      "Homog_daily_max_temp_v2016.zip","Homog_daily_mean_temp_v2016.zip",
      "Homog_daily_min_temp_v2016.zip","Homog_temperature_documentation_v2016.doc",
      "Homog_temperature_stations_v2016.xlsx",
      "Vincent_and_coauthors_Trends_in _Canada's_Climate_JClimate_June2015.pdf",
      "Vincent_et al_Second_Generation_Homog_Temp_JGR_2012.pdf",
      "Wang_Feng_Vincent_Extreme_Temp_AO_2013.pdf",
      "ZMekis_Vincent_2011.pdf"),
    c("NFDB_poly.zip","NFDB_poly_20160712_large_fires_metadata.pdf",
      "NFDB_poly_20160712_metadata.pdf","NFDB_poly_large_fires.zip"),
    c("FR_NFI_and_kNN_Mapping_20160628.docx","NFI_and_kNN_Mapping_20160628.docx",
      "cjfr-2013-0401suppl.pdf", "kNN-EcozonesDomain.zip","kNN-Genus.tar",
      "kNN-LandCover.tar", "kNN-Soils.tar","kNN-Species.tar","kNN-SpeciesDominant.tar",
      "kNN-SpeciesGroups.tar","kNN-StructureBiomass.tar","kNN-StructureStandVolume.tar"),
    c("m", "n", "v"),
    c("LandCoverOfCanada2005_V1_4.zip"),
    c("Land_Cover_2005v2_TIFF.zip"),
    c("Land_Cover_2010_TIFF.zip"),
    c(""))
)

