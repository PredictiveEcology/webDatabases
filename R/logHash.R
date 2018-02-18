################################################################################
#' Write \code{hashdata} to SQLite database
#'
#' If the SQLite database already exists, \code{hashdata} argument is append if not already present.
#'
#' @inheritParams hashDownload
#'
#' @param hashdata A \code{data.frame} containing \code{filename}, \code{checksumFile},
#'                 \code{checksumSize} and \code{algorithm} for each file are stored.
#'
#' @return Invoked for its side-effect of logging filenames and checksum values into the SQL database.
#'
#' @author Melina Houle
#' @docType methods
#' @importFrom DBI dbConnect dbExistsTable dbWriteTable dbSendQuery dbFetch dbClearResult dbDisconnect
#' @importFrom RSQLite SQLite
#' @keywords internal
#' @rdname logHash
#' @export
#'
#' @examples
#' \dontrun{
#' destfile <-tempdir()
#' dbHash <- file.path(destfile, "dbHash.sqlite")
#' file.list<- list.files(destfile)
#' hfile <- hList(file.list, destfile, quick = TRUE)
#' logHash(hfile, dbHash)
#' }
logHash <- function(hashdata, dbHash = "dbHash.sqlite") {
  con <- dbConnect(SQLite(), dbHash)
  if (!dbExistsTable(con, "checksum")) {
    dbWriteTable(con, "checksum", .hdf(), overwrite = TRUE, field.types = NULL)
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
