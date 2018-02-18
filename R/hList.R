################################################################################
#' Extract checksum value from file
#'
#' Stores, in a \code{data.frame}, the filename and its checksum value (computed
#' using the \code{\link[digest]{digest}}).
#'
#' @param fList A character vector representing filename.
#'
#' @param destfile A character string giving file to path to file.
#'
#' @inheritParams hashDownload
#'
#' @param csalgorithm A character string representing the algorithms used by the
#'                    digest function. Default is \code{"xxhash64"}.
#'
#' @return \code{data.frame} where filename, checksum value from the object
#'         (checksumFile), checksum value from the combination of filename and file
#'         size and the algorithm used to compute checksum values are stored.
#'
#' @author Melina Houle
#' @docType methods
#' @importFrom digest digest
#' @keywords internal
#' @export
#' @rdname hList
#'
#' @examples
#'   outdir <- tempdir()
#'   file.list <- list.files(outdir)
#'   webDatabases::hList(file.list, destfile = outdir, quick = TRUE)
#'
hList <- function(fList, destfile, quick = FALSE, csalgorithm = "xxhash64") {
  path2file <- lapply(file.path(destfile, fList), function(x) x[!file.info(x)$isdir])
  fList <- unlist(lapply(path2file, function(x) x[file.exists(x), drop = FALSE]))

  if (length(fList) == 0) {
    hdata <- .hdf()
  } else {
    # Extract filenames and hash value from files downloaded locally.
    if (!quick) {
      # checksum on file
      htag <- lapply(basename(fList), function(i) digest(file = file.path(destfile, i), algo = csalgorithm))

      # checksum on folder using folder name and size
      flst <- list.files(destfile)
      flst <- file.path(destfile,flst)
      allFiles <- lapply(flst, function(x) {list(basename(x), file.info(x)[,"size"])})

      hfolder <- digest(allFiles, algo = csalgorithm)
      checkfolder <- data.frame(Filename = basename(destfile), checksumFile = NA,
                                checksumSize = unlist(hfolder), algorithm = csalgorithm,
                                stringsAsFactors = FALSE)

      # checksum on files using filename and size
      fls <- subset(allFiles, unlist(allFiles)[ c(TRUE,FALSE) ] %in% basename(fList))
      hfile <- lapply(fls, function(i) digest(i, algo = csalgorithm))

      # Fill hdata data.frame
      checkfiles <- data.frame(Filename = basename(fList), checksumFile = unlist(htag),
                               checksumSize = unlist(hfile), algorithm = csalgorithm,
                               stringsAsFactors = FALSE)
      hdata <- rbind(checkfiles, checkfolder)
    } else {
      # checksum on folder using folder name and size
      flst <- list.files(destfile)
      flst <- file.path(destfile, flst)
      allFiles <- lapply(flst, function(x) {list(basename(x), file.info(x)[, "size"])})

      hfolder <- digest(allFiles, algo = csalgorithm)
      checkfolder <- data.frame(Filename = basename(destfile), checksumFile = NA,
                                checksumSize = unlist(hfolder), algorithm = csalgorithm,
                                stringsAsFactors = FALSE)
      # checksum on files using ilename and size
      fls <- subset(allFiles, unlist(allFiles)[ c(TRUE,FALSE) ] %in% basename(fList))
      hfile <- lapply(fls, function(i) {digest(i, algo = csalgorithm)})

      # Fill hdata data.frame
      checkfiles <- data.frame(Filename = basename(fList), checksumFile = NA,
                               checksumSize = unlist(hfile), algorithm = csalgorithm,
                               stringsAsFactors = FALSE)
      hdata <- rbind(checkfiles, checkfolder)
    }
  }
  return(hdata)
}
