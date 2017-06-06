#' Log checksum from downloaded files into a SQLite database
#'
#' Log compiled checksum values from a specific file into a predetermine dbHash
#' sqlite database following SQL language requirements.
#'
#' @param hashdata A \code{data.frame} containing \code{filename}, \code{checksumFile},
#'                 \code{checksumSize} and \code{algorithm} for each file are stored.
#'
#' @param dbHash A character string indicating the name of the database where checksum values are stored.
#'               If the database does not exist, the function will create it and checksum will be computed.
#'               Default is \code{"dbHash.sqlite"}
#'               .
#' @return Invoked for its side-effect of logging filenames and checksum values into the SQL database.
#'
#' @importFrom DBI dbConnect dbExistsTable dbWriteTable dbSendQuery dbFetch dbClearResult dbDisconnect
#' @importFrom RSQLite SQLite
#' @export
#' @docType methods
#' @author Melina Houle
#' @examples
#' destfile <-tempdir()
#' dbHash <- file.path(destfile, "dbHash.sqlite")
#' file.list<- list.files(destfile)
#' hfile <- hList(file.list, destfile, quick = TRUE)
#' logHash(hfile, dbHash)
#'
logHash <- function(hashdata, dbHash = "dbHash.sqlite") {
  con <- dbConnect(SQLite(), dbHash)
  if (!dbExistsTable(con, "checksum")) {
    dbWriteTable(con, "checksum",
                 data.frame(Filename = character(), checksumFile = character(),
                            checksumSize = character(), algorithm = character(),
                            stringsAsFactors = FALSE),
                 overwrite = TRUE, field.types = NULL)
  }
  w <- hashdata$Filename
  # Some file names use quote (') in there names.
  # To fill SQL language requirements, quote should be double ('').
  w <- gsub("'", "''", w)
  # Build a string using filename from current download.
  t <- paste0("'", w, "'", collapse = ",")
  # Delete duplicate rows from database table
  options(warn = -1)
  l <- dbSendQuery(con, paste0(" DELETE FROM checksum WHERE Filename IN (", t, ")"))
  dbFetch(l)
  dbClearResult(l)
  options(warn = 0)
  dbWriteTable(con, "checksum", hashdata, append = TRUE)
  dbDisconnect(con)
}
