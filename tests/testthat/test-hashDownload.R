test_that("test-hashDownload", {
  library(DBI)
  library(digest)
  library(tools)

  on.exit({
    detach("package:DBI")
    detach("package:digest")
    detach("package:tools")
  }, add = TRUE)

  outdir <- tempdir()
  url <- "http://ftp.geogratis.gc.ca/pub/nrcan_rncan/archive/vector/cli_itc_50k/land_use/L040J03.zip"
  dbHash = file.path(outdir, "dbHash.sqlite")

 # Delete file if exists prior to download
  if (file.exists(file.path(outdir, basename(url)))) {
    file.remove(file.path(outdir, basename(url)))
  }
  expect_false(file.exists(file.path(outdir, basename(url))))

  # Run hashDownload
  hashDownload(url, outdir, checkhash = FALSE, quick = FALSE,
               dbHash = dbHash, cascade = FALSE)

  # Test if download occured
  expect_true(file.exists(file.path(outdir, basename(url))))

  # Test checkhash argument
  # DB shouldn't exist.
  expect_false(file.exists(file.path(outdir, dbHash)))

  # Run hashDownload with checkhash = TRUE,  quick = TRUE
  hashDownload(url, outdir, checkhash = TRUE, quick = TRUE)

  # Test if database exist
  expect_true(file.exists(dbHash))

  # Check logged checksum with quick = TRUE.
  con <- dbConnect(RSQLite::SQLite(), dbHash)
  hfile <- dbReadTable(con, "checksum")
  # Test if filename logged in db
  loggedFilename <- hfile[hfile$Filename == basename(url), 1]
  expect_true(isTRUE(loggedFilename == basename(url)))

  # Test checksumSize value with manual digest on filename + file size
  logchecksumsize <- hfile[which(hfile$Filename == basename(url)), "checksumSize"]

  digestsize <- list(basename(url), file.info(file.path(outdir, basename(url)))[, "size"])
  algo = "xxhash64"
  checksum <- digest(digestsize, algo = algo)

  expect_identical(logchecksumsize, checksum)

  # With quick = TRUE, checksumFile value should be NA.
  logchecksumfile <- hfile[which(hfile$Filename == basename(url)), "checksumFile"]
  expect_true(is.na(logchecksumfile))


  # Test hashDownload. Shouldn't redownload. File already exists.
  dwdTime <- file.info(file.path(outdir, basename(url)))[, "mtime"]
  time <- Sys.time()
  hashDownload(url, outdir, checkhash = TRUE, quick = TRUE,
               dbHash = dbHash , cascade = FALSE)
  expect_true(dwdTime < time)

  # Delete checksum from db
  todelete <- dbSendQuery(con, " DELETE FROM checksum WHERE Filename = 'L040J03.zip'")
  dbClearResult(todelete)
  # Make sure entry is deleted
  hfile <- dbReadTable(con, "checksum")
  loggedFilename <- hfile[hfile$Filename == basename(url), 1]
  expect_false(isTRUE(loggedFilename == basename(url)))

  # Test hashDownload. Should redownload because checkhash is TRUE and checksum is missing
  hashDownload(url, outdir, checkhash = TRUE, quick = TRUE,
               dbHash = dbHash , cascade = FALSE)

  # Test hashDownload. Shouldn't redownload. File already exists and checksum is logged.
  hashDownload(url, outdir, checkhash = TRUE,
               dbHash = dbHash , cascade = FALSE, quick = TRUE)

  # Delete file
  expect_true(file.remove(file.path(outdir, basename(url))))

  # Test hashDownload. Should redownload because file is missing
  hashDownload(url, outdir, checkhash = TRUE, quick = TRUE,
               dbHash = dbHash , cascade = FALSE)

  # Test logged checksumFile with quick = FALSE. File will redownload to log the proper checksum.
  hashDownload(url, outdir, checkhash = TRUE, quick = FALSE,
          dbHash = dbHash , cascade = FALSE)

  con <- dbConnect(RSQLite::SQLite(), dbHash)
  hfile <- dbReadTable(con, "checksum")

  logchecksumfile <- hfile[which(hfile$Filename == basename(url)), 'checksumFile']

  # With quick = TRUE, checksumFile value should be NA.
  expect_true(!is.na(logchecksumfile))

  # Test checksum value manually
  checksum <- digest(file = file.path(outdir, basename(url)), algo = algo)
  expect_identical(logchecksumfile, checksum)
  dbDisconnect(con)

  # Delete file and test if redownload occurs
  file.remove(file.path(outdir, basename(url)))
  expect_false(file.exists(file.path(outdir, basename(url))))
  hashDownload(url, outdir, checkhash = TRUE, quick = FALSE,
               dbHash = dbHash , cascade = TRUE)

  # Delete file
  expect_true(file.remove(file.path(outdir, basename(url))))
  # Test cascade
  expect_false(file.exists(file.path(outdir, basename(url))))
  hashDownload(url,outdir, checkhash = FALSE, quick = FALSE,
               dbHash = dbHash , cascade = TRUE)

  expect_true(file.exists(file.path(outdir, basename(url))))
  expect_equal(length(list.files(file.path(outdir, file_path_sans_ext(basename(url))))), 2)
})
