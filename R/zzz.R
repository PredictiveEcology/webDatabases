## -------------------------------------------------------------------------- ##
## edit the package options help documentation in webDatabases-package.R      ##
## -------------------------------------------------------------------------- ##

.onLoad <- function(libname, pkgname) {
  ## set options using the approach used by devtools
  opts <- options()
  opts.webDatabases <- list( # nolint
    webdatabases.dbfile = "dbHash.sqlite"
  )
  toset <- !(names(opts.webDatabases) %in% names(opts))
  if (any(toset)) options(opts.webDatabases[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage("Using webDatabases version ", utils::packageVersion("webDatabases"), ".")
  }
}
