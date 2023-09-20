## Push to www.nifustaging.net/stephan

options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))) # Original: etc/ca-bundle.crt
withr::with_dir(new = "_site", code = {
  fs::dir_ls(recurse = TRUE, all = TRUE, type = "file") |>
    purrr::map(.f = ~{
      library(RCurl)
      print(.x)
      Sys.sleep(time = 1)
      tryCatch(expr = RCurl::ftpUpload(what = .x, 
                       to = paste0("ftp://cpanel80.proisp.no/", .x),
                       userpwd = "stephan@nifustaging.net:2p{eUqdquHN2",
                       .opts = list(ftp.create.missing.dirs=TRUE, ftp.ssl = TRUE,
                                    ssl.verifypeer = FALSE)),
               error = function(e) e)
    })
})


tryCatch(RCurl::ftpUpload(what = "_site/index.html", 
                 to = "ftp://cpanel80.proisp.no/index.html",
                 userpwd = "stephan@nifustaging.net:2p{eUqdquHN2",
                 .opts = list(ftp.create.missing.dirs=TRUE, ftp.ssl = TRUE,
                              ssl.verifypeer = FALSE)),
         error = function(e) e) 
