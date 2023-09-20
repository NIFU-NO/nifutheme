readxl::read_excel("C:\\Users\\py128\\OneDrive - NIFU\\kontaktinfo.xlsx") %>%
  labelled::set_variable_labels(.labels = c("Databaseløpenummer", "Databaseløpenummer2", "GSI-id", "Organisasjonsnummer", "Skolens fulle navn", 
                                            "Epostadresse", "Spørringene er delt i 3 like utvalg som roteres", "Dato for siste oppdatering", "Kommunens navn", "Kommunenummer", "Fylkenavn", "Fylkenummer", "Type skole, firedelt", "Notat")) %>%
  mutate(sk4d = ifelse(sk4d == "Ungdomskole", "Ungdomsskole", sk4d),
         sk4d = as.integer(factor(sk4d, levels=c("Barneskole", "Ungdomsskole", "1-10 skole", "Videregående")))) %>% 
  labelled::set_value_labels(sk4d = c("Barneskole"=1, "Ungdomsskole"=2, "1-10 skole"=3, "Videregående"=4)) %>% tabyl_labelled(sk4d)
  haven::write_dta(data = ., path = paste0(paths$dat_udir, "/sporringene_database_uttrekk.dta"))
