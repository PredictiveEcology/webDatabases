test_that("test-hashDownload", {

  library(SpaDES)
  library(DBI)
  library(digest)
  library(tools)

   on.exit({
    detach("package:SpaDES")
    detach("package:DBI")
     detach("package:digest")
     detach("package:tools")
  }, add = TRUE)

  outdir<-tempdir()
  url<-"ftp://ccrp.tor.ec.gc.ca/pub/EC_data/AHCCD_daily/ZMekis_Vincent_2011.pdf"
  dbHash = file.path(outdir, "dbHash.sqlite")
  algo = "xxhash64"
  sim <- SpaDES::simInit(times = list(start = 2010.0, end = 2020.0, timeunit = "year"),
                         params = list(),
                         modules = list(),
                        paths = list(outputPath=outdir))


  sim <- SpaDES::simInit()

  # Delete file if exists prior to download
  if(file.exists(file.path(outdir, basename(url)))){
    file.remove(file.path(outdir, basename(url)))
  }
  expect_false(file.exists(file.path(outdir, basename(url))))

  # Run hashDownload
  hashDownload(url, outdir, sim, module, checkhash = FALSE, quick = FALSE,
                       dbHash = dbHash, cascade= FALSE)

  # Test if download occured
  expect_true(file.exists(file.path(outdir, basename(url))))

    # Test checkhash argument
  # DB shouldn't exist.
  expect_false(file.exists(file.path(outdir, dbHash)))

  # Run hashDownload with checkhash = TRUE,  quick = TRUE
  hashDownload(url, outdir, sim, module, checkhash = TRUE, quick = TRUE,
          dbHash = dbHash , cascade= FALSE)

  # Test if database exist
  expect_true(file.exists(dbHash))

  # Check logged checksum with quick = TRUE.
  con = dbConnect(RSQLite::SQLite(), dbHash)
  hfile <-dbReadTable(con, "checksum")
  # Test if filename logged in db
  loggedFilename <- hfile[hfile$Filename == basename(url),1]
  expect_true(isTRUE(loggedFilename==basename(url)))

  # Test checksumSize value with manual digest on filename + file size
  logchecksumsize<- hfile[which(hfile$Filename == basename(url)), 'checksumSize']

  digestsize <- list(basename(url), file.info(file.path(outdir, basename(url)))[,"size"])
  checksum <- digest(digestsize, algo = algo)

  expect_identical(logchecksumsize, checksum)

  # With quick = TRUE, checksumFile value should be NA.
  logchecksumfile<- hfile[which(hfile$Filename == basename(url)), 'checksumFile']
  expect_true(is.na(logchecksumfile))

  # Test hashDownload. Shouldn't redownload. File already exists.
  hashDownload(url, outdir, sim, module, checkhash = TRUE, quick = TRUE,
          dbHash = dbHash , cascade= FALSE)

  # Delete checksum from db
  todelete <- dbSendQuery(con, " DELETE FROM checksum WHERE Filename = 'ZMekis_Vincent_2011.pdf'")
  dbClearResult(todelete)
  # Make sure entry is deleted
  hfile <-dbReadTable(con, "checksum")
  loggedFilename <- hfile[hfile$Filename == basename(url),1]
  expect_false(isTRUE(loggedFilename==basename(url)))

  # Test hashDownload. Should redownload because checkhash is TRUE and checksum is missing
  hashDownload(url, outdir, sim, module, checkhash = TRUE, quick = TRUE,
          dbHash = dbHash , cascade= FALSE)

  # Test hashDownload. Shouldn't redownload. File already exists and checksum is logged.
  hashDownload(url, outdir, sim, module, checkhash = TRUE,
          dbHash = dbHash , cascade= FALSE, quick = TRUE)

  # Delete file
  expect_true(file.remove(file.path(outdir, basename(url))))

  # Test hashDownload. Should redownload because file is missing
  hashDownload(url, outdir, sim, module, checkhash = TRUE, quick = TRUE,
          dbHash = dbHash , cascade= FALSE)


  # Test logged checksumFile with quick = FALSE. File will redownload to log the proper checksum.
  hashDownload(url, outdir, sim, module, checkhash = TRUE, quick = FALSE,
          dbHash = dbHash , cascade= FALSE)

  con = dbConnect(RSQLite::SQLite(), dbHash)
  hfile <-dbReadTable(con, "checksum")

  logchecksumfile<- hfile[which(hfile$Filename == basename(url)), 'checksumFile']

  # With quick = TRUE, checksumFile value should be NA.
  expect_true(!is.na(logchecksumfile))

  # Test checksum value manually
  checksum <- digest(file= file.path(outdir, basename(url)), algo = algo)
  expect_identical(logchecksumfile, checksum)
  dbDisconnect(con)


  # Delete file and test if redownload occurs
  file.remove(file.path(outdir, basename(url)))
  expect_false(file.exists(file.path(outdir, basename(url))))
  hashDownload(url, outdir, sim, module, checkhash = TRUE, quick = FALSE,
          dbHash = dbHash , cascade= TRUE)


  # Test cascade
  url = "ftp://ccrp.tor.ec.gc.ca/pub/EC_data/AHCCD_daily/Adj_Daily_Snow_v2016.zip"
  expect_false(file.exists(file.path(outdir, basename(url))))
  hashDownload(url,outdir, sim, module, checkhash = FALSE, quick = FALSE,
          dbHash = dbHash , cascade= TRUE)

  expect_true(file.exists(file.path(outdir, basename(url))))
  expect_equal(length(list.files(file.path(outdir, file_path_sans_ext(basename(url))))), 463)

})
