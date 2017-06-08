#' Template data.frame to use for file hash table
#'
#' Internal function. Returns an empty \code{data.frame} with columns:
#' \code{Filename}, \code{checksumFile}, \code{checksumSize}, and \code{algorithm}.
#'
#' @author Alex Chubaty
#' @docType methods
#' @keywords internal
#' @rdname internal
.hdf <- function() {
  data.frame(Filename = character(0), checksumFile = character(0),
             checksumSize = character(0), algorithm = character(0),
             stringsAsFactors = FALSE)
}
