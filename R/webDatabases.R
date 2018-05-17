#' Table of relevant dataset accessible using url
#'
#' Stores dataset name with their respective url and username/password.
#' The table is used by \code{listWebDatabases} to retrive file available to
#' download by using only the dataset name.
#'
#' @return \code{data.table} containing dataset available for download.
#'
#' @author Melina Houle
#' @docType methods
#' @importFrom data.table data.table
#' @keywords internal
#' @export
#' @rdname webdataset
#'
urlsWide <- function() {
  data.table(
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
            "http://tree.pfc.forestry.ca/",
            "ftp://ftp.nofc.cfs.nrcan.gc.ca/uploads/MPB/",
            "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/",
            "http://www.cec.org/sites/default/files/Atlas/Files/Land_Cover_2005/",
            "http://www.cec.org/sites/default/files/Atlas/Files/Land_Cover_2010/",
            "http://tree.pfc.forestry.ca/"),
    password = c(NA,
                 NA,
                 NA,
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
}
