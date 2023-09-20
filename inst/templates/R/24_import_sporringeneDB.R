library(dbplyr)
conn_sporringene_skoleledere <-
  "C:/Users/py128/OneDrive - NIFU/Skrivebord/kontaktinfo_skole.accdb" %>%
  paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", .) %>%
  DBI::dbConnect(odbc::odbc(), .connection_string = .)
DBI::dbListTables(conn_sporringene_skoleledere)
dat_sporringene_skoleledere <-
  DBI::dbReadTable(conn=conn_sporringene_skoleledere, 
                   name = "kontaktinfo") %>%
  tibble::as_tibble()
