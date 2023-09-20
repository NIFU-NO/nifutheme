#Tabeller våren 2023, my allegiance is for the republic, for democracy!
library("tidyverse")
library("surveyreport")
library("haven")
library("readSX")
library("crosstable")
library("tibble")
library("sf")
library("rjson")
library("maps")
#install.packages("maps")
# install and load the mapdata package
#install.packages("mapdata")
library("mapdata")
#install.packages("igraph")
library("igraph")
library(labelled)

options(repos = "https://cran.r-project.org")

Utv_data <- read_dta("C:/Users/TorsteinDeBesche/NIFU/21206 Utdanningsdirektoratets spørringer - Data - Data/Survey/V2023/ferdige filer/utvalg_final.dta") %>% 
  mutate(across(where(~labelled::is.labelled(.x)), ~labelled::to_factor(.x))) %>% 
  mutate(across(where(~is.numeric(.x)), ~labelled::to_factor(.x)))

#resp = 1 (skoleleder grunnskole), resp = 2 (skoleleder vgs), resp = 3 (skoleeier kommune), resp = 4 (skoleeier fylke)

Pop_data <- read_dta("C:/Users/TorsteinDeBesche/NIFU/21206 Utdanningsdirektoratets spørringer - Data - Data/Survey/V2023/endelig utvalg/pop_alle.dta") %>% 
  mutate(across(where(~labelled::is.labelled(.x)), ~labelled::to_factor(.x))) %>% 
  mutate(across(where(~is.numeric(.x)), ~labelled::to_factor(.x)))  #Har med disse mutate setningene for å gjøre om alle variabler til samme type som Utv_data datasettet, one time thing.

Hel_data <- bind_rows(Pop_data = Pop_data, Utv_data = Utv_data, .id = "kilder") %>% 
  labelled::copy_labels_from(from=Utv_data, .strict = FALSE)

#Gjør antall svar om til numeric
Hel_data$Antall_svar <- as.numeric(levels(Hel_data$Antall_svar))[Hel_data$Antall_svar]

#lager godkjent numeric variable, men bruker den ikke ofte
Hel_data <- Hel_data %>% 
  mutate(godkjent1 = case_when(
    godkjent == 1 ~ "Godkjent",
    godkjent == 0 ~ "Ikke godkjent",
    TRUE ~ NA_character_
  ))

Hel_data <- Hel_data %>% 
  mutate(valgt = case_when(
    kilder == "Pop_data" ~ 0,
    kilder == "Utv_data" ~ 1
  ))

#MÅ fikse noen variabler navn/verdier
Hel_data <- Hel_data %>% 
  mutate(resp = case_when(
    resp == "Skoleleder grunnskole" ~ "Skoleleder grunnskole",
    resp == "Skoleleder videregående" ~ "Skoleleder videregående",
    resp == "Skoleeier kommmune" ~ "Skoleeier kommune",
    resp == "Skoleeier kommune" ~ "Skoleeier kommune",
    resp == "Skoleeier fylke" ~ "Skoleeier fylke"
  ))


Hel_data <- Hel_data %>% 
  mutate(resp1 = case_when(
    resp == "Skoleleder grunnskole" ~ 1,
    resp == "Skoleleder videregående" ~ 2,
    resp == "Skoleeier kommune" ~ 3,
    resp == "skoleeier fylke" ~ 4,
    #TRUE ~ NA_integer_
  ))

#Orgform fiks:

Hel_data <- Hel_data %>% 
  mutate(orgform = case_when(
    orgform == 1 ~ "Offentlig",
    orgform == 2 ~ "Privat",
    orgform == "Offentlig" ~ "Offentlig",
    orgform == "Privat" ~ "Privat"
  ))

#sk4d fiks:
Hel_data <- Hel_data %>% 
  mutate(sk4d = case_when(
    sk4d == "Barneskole" ~ "Barneskole",
    sk4d == "Ungdomskole" ~ "Ungdomskole",
    sk4d == "Ungdomsskole" ~ "Ungdomskole",
    sk4d == "1-10 skole" ~ "1-10 skole",
    sk4d == "1-10. skoler" ~ "1-10 skole",
    sk4d == "Videregående" ~ "Videregående"
  ))

#Lager numerisk godkjent variabel
Hel_data <- Hel_data %>% 
  mutate(godkjent1 = case_when(
    godkjent == "Godkjent" ~ 1,
    godkjent == "Ikke godkjent" ~ 0
  ))

#Gjør landsdel_gs til character
Hel_data <- Hel_data %>% 
  mutate(across(c(landsdel_gs), ~ labelled::to_character(.x)))

#kstør_gs og kstør_vgs og kstør_kom typo fix


Hel_data <- Hel_data %>%
  mutate(kstør_gs = if_else(kstør_gs == "Mer enn 20 0000", "Mer enn 20 000", kstør_gs))

Hel_data <- Hel_data %>%
  mutate(kstør_vgs = if_else(kstør_vgs == "Mer enn 20 0000", "Mer enn 20 000", kstør_vgs))

Hel_data <- Hel_data %>%
  mutate(kstør_kom = if_else(kstør_kom == "Mer enn 20 0000", "Mer enn 20 000", kstør_kom))





#Tabell 2.2 (fra h2022 labels)
tab2.2 <- Hel_data %>% 
  filter(kilder == "Utv_data") %>% 
  mutate(across(c(resp, svarstatus), ~ as.factor(.x))) %>% 
  crosstable::crosstable(cols =resp, 
                         by = svarstatus, showNA = "no", percent_pattern = "{n}", total = "both") %>% 
  select(-.id, -label) 


#Riktig, samme rekkefølge problem som 2.2
tab2.3 <- Hel_data %>% 
  filter(kilder == "Utv_data") %>% 
  mutate(Antall_svar = as.numeric(Antall_svar)) %>%
  group_by(resp, godkjent) %>%
  summarize(Gjennomsnitt = mean(Antall_svar, na.rm = TRUE),
            Median = median(Antall_svar, na.rm = TRUE),
            Maks = max(Antall_svar, na.rm = TRUE))

#Riktig, usikker på om group_by må stå to ganger, men tror det.
tab2.4 <- 
  Hel_data %>% 
  mutate(beskriv = case_when(
    kilder == "Utv_data" ~ "Utvalg",
    kilder == "Pop_data" ~ "Populasjon"
  )) %>% 
  group_by(resp) %>% 
  mutate(Maks_populasjon = sum(beskriv == "Populasjon")) %>% 
  group_by(beskriv, resp) %>% 
  mutate(Bruttoutvalg = n()) %>%
  summarize(Godkjente_svar = sum(godkjent == "Godkjent", na.rm = TRUE),
            Bruttoutvalg = unique(Bruttoutvalg),
            svarprosent_bruttoutvalg = (Godkjente_svar / Bruttoutvalg) * 100,
            Maks_populasjon = unique(Maks_populasjon),
            Andel_populasjon_deltatt = ifelse(Maks_populasjon == 0, 0, (Godkjente_svar / Maks_populasjon) * 100),
            .groups = "drop") %>% 
  pivot_longer(cols = c("Godkjente_svar", "Bruttoutvalg", "svarprosent_bruttoutvalg", "Maks_populasjon", "Andel_populasjon_deltatt"), 
               names_to = "Variable", values_to = "Value") %>% 
  pivot_wider(names_from =  resp, values_from = "Value") %>% 
  relocate("Skoleeier fylke", .after = tidyselect::last_col()) %>% 
  relocate("Skoleeier kommune", .before = "Skoleeier fylke")  %>%
  filter(!(beskriv == "Populasjon" & Variable %in% c("Godkjente_svar", "svarprosent_bruttoutvalg", "Andel_populasjon_deltatt"))) %>% #Her filtrerer jeg rader som er null, for eksempel Populasjon og godkjente svar (Ingen fra populasjonen har svart på spørsmål så ingen kan være godkjent)
  slice(c(4, 2, 3, 5, 7)) %>% 
  mutate(across(.cols = where(is.numeric), .fns = ~round(., 1)))

#Riktig
tab2.5 <- Hel_data %>%
  filter(resp1 == 1) %>% 
  mutate(godkjent = if_else(godkjent == "Godkjent", 1, 0),
         pop = if_else(kilder == "Pop_data", 1, 0),
         ut = if_else(kilder == "Utv_data", 1, 0)) %>% 
  group_by(fylke) %>% 
  summarize(Populasjon = sum(pop, na.rm = TRUE), 
            Utvalg = sum(ut, na.rm = TRUE),
            Antall_svar = sum(godkjent, na.rm = TRUE),
            svarprosent = round(Antall_svar / Utvalg * 100, digits = 1)) %>% 
  bind_rows(summarize(., fylke = "Totalt", 
                      Populasjon = sum(Populasjon),
                      Utvalg = sum(Utvalg),
                      Antall_svar = sum(Antall_svar),
                      svarprosent = round(sum(Antall_svar) / sum(Utvalg) * 100, digits = 1)))

#Helt riktig, bind_rows kan være før og etter group_by uten at det gjør noe forskjell. Men kan ikke være etter summarize (of course)
tab2.6 <- Hel_data %>% 
  filter(resp1 == 1) %>% 
  mutate(#landsdel_gs = if_else(kilder == "Pop_data", "Totalt", landsdel_gs),
    godkjent = if_else(godkjent == "Godkjent", 1, 0)) %>% 
  bind_rows(., mutate(., sk4d = "Totalt"), 
            mutate(., landsdel_gs = "Totalt"),
            mutate(., sk4d = "Totalt", landsdel_gs = "Totalt")) %>%  
  group_by(landsdel_gs, sk4d) %>% 
  summarize(svarprosent = round(ifelse(sum(valgt, na.rm = TRUE) == 0, 0, sum(godkjent, na.rm = TRUE)/sum(valgt, na.rm = TRUE)*100), digits = 1)) %>% 
  pivot_wider(names_from = sk4d, values_from = svarprosent) %>% 
  relocate("1-10 skole", .after = "Ungdomskole") %>%
  relocate("Totalt", .after = last_col()) %>% 
  filter(!is.na(landsdel_gs)) %>%   #Uten denne får vi en rad som står som NA på landsdel_gs kolonnen og er 0 på alle de andre kolonnene.
  ungroup() %>% 
  slice(c(2, 5, 3, 1, 4))


#Riktig
tab2.7 <- Hel_data %>% 
  filter(resp1 == 1) %>% 
  mutate(#landsdel_gs = if_else(kilder == "Pop_data", "Totalt", landsdel_gs),
    godkjent = if_else(godkjent == "Godkjent", 1, 0)) %>% 
  bind_rows(., mutate(., gs3d = "Totalt"), 
            mutate(., landsdel_gs = "Totalt"),
            mutate(., gs3d = "Totalt", landsdel_gs = "Totalt")) %>%  
  group_by(landsdel_gs, gs3d) %>% 
  summarize(svarprosent = round(ifelse(sum(valgt, na.rm = TRUE) == 0, 0, sum(godkjent, na.rm = TRUE)/sum(valgt, na.rm = TRUE)*100), digits = 1)) %>% 
  pivot_wider(names_from = gs3d, values_from = svarprosent) %>% 
  relocate("Totalt", .after = last_col()) %>% 
  relocate("Under 100", .before = "100-299") %>% 
  filter(!is.na(landsdel_gs)) %>%
  ungroup() %>% 
  slice(c(2, 5, 3, 1, 4))

tab2.8U <- Hel_data %>% 
  filter(godkjent == "Godkjent",
         sk4d != "Videregående") %>% 
  crosstable::crosstable(cols = landsdel_gs, by = sk4d, 
                         percent_pattern = list(body="{p_tot}", total_row = "{p_col}",
                                                total_col ="{p_row}", total_all = "{p_col}"),
                         showNA = "no", percent_digits = 1, total = "both") %>% 
  relocate(c("Barneskole", "Ungdomskole", "1-10 skole"), .before = last_col()) %>% 
  rename("1-10 skole utvalg" = "1-10 skole",
         "Ungdomskole utvalg" = "Ungdomskole",
         "Barneskole utvalg" = "Barneskole",
         "Totalt utvalg" = "Total")

tab2.8P <- Hel_data %>% 
  filter(sk4d != "Videregående") %>% 
  crosstable::crosstable(cols = landsdel, by = sk4d, percent_pattern = list(body="{p_tot}", total_row = "{p_col}",
                                                                            total_col ="{p_row}", total_all = "{p_col}"),
                         showNA = "no", percent_digits = 1, total = "both") %>% 
  relocate(c("Barneskole", "Ungdomskole", "1-10 skole"), .before = last_col()) %>% 
  rename("1-10 skole populasjon" = "1-10 skole",
         "Ungdomskole populasjon" = "Ungdomskole",
         "Barneskole populasjon" = "Barneskole",
         "Totalt populasjon" = "Total")
#Riktig.
tab2.8 <- inner_join(tab2.8P, tab2.8U, by = "variable") %>% 
  rename("Landsdel" = "variable") %>% 
  relocate(c("Landsdel", "Barneskole utvalg", "Barneskole populasjon", "Ungdomskole utvalg", "Ungdomskole populasjon",
             "1-10 skole utvalg", "1-10 skole populasjon", "Totalt utvalg", "Totalt populasjon")) %>% 
  select(-.id.x, -.id.y, -label.x, -label.y)

#Utvalg
tab2.9U <- Hel_data %>% 
  filter(godkjent == "Godkjent",
         sk4d != "Videregående") %>% 
  crosstable::crosstable(cols = landsdel_gs, by = gs3d, percent_pattern = list(body="{p_tot}", total_row = "{p_col}",
                                                                               total_col ="{p_row}", total_all = "{p_col}"),
                         showNA = "no", percent_digits = 1, total = "both") %>% 
  rename("Under 100 utvalg" = "Under 100",
         "100-299 utvalg" = "100-299",
         "300 og over utvalg" = "300 og over",
         "Totalt utvalg" = "Total")
#Populasjon
tab2.9P <- Hel_data %>% 
  filter(sk4d != "Videregående") %>% 
  crosstable::crosstable(cols = landsdel, by = gs3d, percent_pattern = list(body="{p_tot}", total_row = "{p_col}",
                                                                            total_col ="{p_row}", total_all = "{p_col}"),
                         showNA = "no", percent_digits = 1, total = "both") %>% 
  rename("Under 100 populasjon" = "Under 100",
         "100-299 populasjon" = "100-299",
         "300 og over populasjon" = "300 og over",
         "Totalt populasjon" = "Total")
#RIKTIG
tab2.9 <- inner_join(tab2.9P, tab2.9U, by = "variable") %>% 
  select(-.id.x, -.id.y, -label.x, -label.y) %>% 
  rename("Landsdel" = "variable") %>% 
  relocate(c("Landsdel", "Under 100 utvalg", "Under 100 populasjon", "100-299 utvalg", "100-299 populasjon", "300 og over utvalg", "300 og over populasjon",
             "Totalt utvalg", "Totalt populasjon"))

tab2.10.orgform <- Hel_data %>% 
  filter(resp1 == 1) %>% 
  group_by(orgform) %>% 
  summarize(Populasjon = sum(kilder == "Pop_data", na.rm = TRUE),
            Nettoutvalg = sum(godkjent == "Godkjent", na.rm = TRUE)) %>% 
  mutate(PopulasjonProsent = round(Populasjon / sum(Populasjon)*100, digits = 1),
         NettoutvalgProsent= round(Nettoutvalg / sum(Nettoutvalg) *100, digits = 1)) %>% 
  select(-Populasjon, -Nettoutvalg)


tab2.10.målform <- Hel_data %>% 
  filter(resp1 == 1) %>% 
  group_by(målform) %>% 
  summarize(Populasjon = sum(kilder == "Pop_data", na.rm = TRUE),
            Nettoutvalg = sum(godkjent == "Godkjent", na.rm = TRUE)) %>% 
  mutate(PopulasjonProsent = round(Populasjon / sum(Populasjon) *100, digits = 1),
         NettoutvalgProsent = round(Nettoutvalg / sum(Nettoutvalg) *100, digits = 1)) %>% 
  select(-Populasjon, -Nettoutvalg)


#
tab2.10 <- bind_rows(
  mutate(tab2.10.orgform, Type = orgform),
  mutate(tab2.10.målform, Type = målform)
) %>% 
  select(-orgform, -målform) %>% 
  relocate(PopulasjonProsent, .after = Type) %>% 
  relocate(NettoutvalgProsent, .after = PopulasjonProsent)

#ENDELIG en tabell som er helt riktig.
tab2.11 <- Hel_data %>%
  filter(resp1 == 2) %>% 
  mutate(godkjent = if_else(godkjent == "Godkjent", 1, 0),
         pop = if_else(kilder == "Pop_data", 1, 0),
         ut = if_else(kilder == "Utv_data", 1, 0)) %>% 
  group_by(fylke) %>% 
  summarize(Populasjon = sum(pop, na.rm = TRUE), 
            Utvalg = sum(ut, na.rm = TRUE),
            Antall_svar = sum(godkjent, na.rm = TRUE),
            svarprosent = round(Antall_svar / Utvalg * 100, digits = 1)) %>% 
  bind_rows(summarize(., fylke = "Totalt", 
                      Populasjon = sum(Populasjon),
                      Utvalg = sum(Utvalg),
                      Antall_svar = sum(Antall_svar),
                      svarprosent = round(sum(Antall_svar) / sum(Utvalg) * 100, digits = 1)))

#Utvalg
tab2.12U <- Hel_data %>% 
  filter(godkjent == "Godkjent",
         sk4d == "Videregående") %>% 
  crosstable::crosstable(cols = landsdel_vgs, by = typevgs, percent_pattern = list(body="{p_tot}", total_row = "{p_col}",
                                                                                   total_col ="{p_row}", total_all = "{p_col}"),
                         showNA = "no", percent_digits = 1, total = "both") %>% 
  rename("Studiespes utvalg" = "Skolen har kun studieforberedende studieretninger",
         "Kombinert utvalg" = "Skolen har både studieforberedende- og yrkesfaglige studieretninger",
         "Yrkesfaglig utvalg" = "Skolen har kun yrkesfaglige studieretninger",
         "Totalt utvalg" = "Total") %>% 
  select(-.id, -label, -Studiespesialiserende, -Kombinert, -Yrkesfag)

"Populasjon"
tab2.12P <- Hel_data %>% 
  filter(sk4d == "Videregående",
         kilder == "Pop_data") %>% 
  crosstable::crosstable(cols = landsdel, by = typevgs, percent_pattern = list(body="{p_tot}", total_row = "{p_col}",
                                                                               total_col ="{p_row}", total_all = "{p_col}"),
                         showNA = "no", percent_digits = 1, total = "both") %>% 
  rename("Studiespes populasjon" = "Studiespesialiserende",
         "Kombinert populasjon" = "Kombinert",
         "Yrkesfaglig populasjon" = "Yrkesfag",
         "Totalt populasjon" = "Total") %>% 
  select(-.id, -label, -"Skolen har kun studieforberedende studieretninger", -"Skolen har både studieforberedende- og yrkesfaglige studieretninger",
         -"Skolen har kun yrkesfaglige studieretninger")

#Riktig
tab2.12 <- inner_join(tab2.12P, tab2.12U, by = "variable") %>% 
  rename("Landsdel" = "variable") %>% 
  relocate(c("Landsdel", "Studiespes utvalg", "Studiespes populasjon", "Kombinert utvalg", "Kombinert populasjon",
             "Yrkesfaglig utvalg", "Yrkesfaglig populasjon", "Totalt utvalg", "Totalt populasjon"))

#Utvalg
tab2.13U <- Hel_data %>% 
  filter(godkjent == "Godkjent",
         sk4d == "Videregående") %>% 
  crosstable::crosstable(cols = landsdel_vgs, by = vgs3d, percent_pattern = list(body="{p_tot}", total_row = "{p_col}",
                                                                                 total_col ="{p_row}", total_all = "{p_col}"),
                         showNA = "no", percent_digits = 1, total = "both") %>% 
  rename("Under 250 utvalg" = "Under 250",
         "250-599 utvalg" = "250-599",
         "600 og over utvalg" = "600 og over",
         "Totalt utvalg" = "Total") %>% 
  select(-.id, -label)
#Populasjon
tab2.13P <- Hel_data %>% 
  filter(sk4d == "Videregående",
         kilder == "Pop_data") %>% 
  crosstable::crosstable(cols = landsdel, by = vgs3d, percent_pattern = list(body="{p_tot}", total_row = "{p_col}",
                                                                             total_col ="{p_row}", total_all = "{p_col}"),
                         showNA = "no", percent_digits = 1, total = "both") %>% 
  rename("Under 250 populasjon" = "Under 250",
         "250-599 populasjon" = "250-599",
         "600 og over populasjon" = "600 og over",
         "Totalt populasjon" = "Total") %>% 
  select(-.id, -label)

#Riktig, men 0.x og 0.y? What the fuck is up with that?
tab2.13 <- inner_join(tab2.13U, tab2.13P, by = "variable") %>% 
  rename("Landsdel" = "variable") %>% 
  relocate(c("Landsdel", "Under 250 utvalg", "Under 250 populasjon", "250-599 utvalg", "250-599 populasjon", 
             "600 og over utvalg", "600 og over populasjon", "Totalt utvalg", "Totalt populasjon"))


tab2.14.orgform <- Hel_data %>% 
  filter(resp1 == 2) %>% 
  group_by(orgform) %>% 
  summarize(Populasjon = sum(kilder == "Pop_data", na.rm = TRUE),
            Nettoutvalg = sum(godkjent == "Godkjent", na.rm = TRUE)) %>% 
  mutate(PopulasjonProsent = round(Populasjon / sum(Populasjon)*100, digits = 1),
         NettoutvalgProsent = round(Nettoutvalg /sum(Nettoutvalg)*100, digits = 1)) %>% 
  select(-Populasjon, -Nettoutvalg)

tab2.14.målform <- Hel_data %>% 
  filter(resp1 == 2) %>% 
  group_by(målform) %>% 
  summarize(Populasjon = sum(kilder == "Pop_data", na.rm = TRUE),
            Nettoutvalg = sum(godkjent == "Godkjent", na.rm = TRUE)) %>% 
  mutate(PopulasjonProsent = round(Populasjon / sum(Populasjon) *100, digits = 1),
         NettoutvalgProsent = round(Nettoutvalg / sum(Nettoutvalg) *100, digits = 1)) %>% 
  select(-Populasjon, -Nettoutvalg)

#Populasjon og nettoutvalg har snudd seg i rapporten, samme som i tab2.10
tab2.14 <- bind_rows(  
  mutate(tab2.14.orgform, Type = orgform),
  mutate(tab2.14.målform, Type = målform)
) %>% 
  select(-orgform, -målform) %>% 
  relocate(Type, .before = PopulasjonProsent)

#RIKTIG 
tab2.15 <- Hel_data %>%    
  filter(resp1 == 3) %>% 
  mutate(godkjent = if_else(godkjent == "Godkjent", 1, 0),
         pop = if_else(kilder == "Pop_data", 1, 0),
         ut = if_else(kilder == "Utv_data", 1, 0)) %>% 
  group_by(fylke) %>% 
  summarize(Populasjon = sum(pop, na.rm = TRUE), 
            Utvalg = sum(ut, na.rm = TRUE),
            Antall_svar = sum(godkjent, na.rm = TRUE),
            svarprosent = round(Antall_svar / Utvalg * 100, digits = 1)) %>% 
  bind_rows(summarize(., fylke = "Totalt", 
                      Populasjon = sum(Populasjon),
                      Utvalg = sum(Utvalg),
                      Antall_svar = sum(Antall_svar),
                      svarprosent = round(sum(Antall_svar) / sum(Utvalg) * 100, digits = 1)))


#Måtte endre kategorier her fra h2022
tab2.16 <- Hel_data %>% 
  filter(resp1 == 3) %>% 
  mutate(godkjent = if_else(godkjent == "Godkjent", 1, 0)) %>% 
  bind_rows(., mutate(., kstør_kom = "Totalt"), 
            mutate(., landsdel_kom = "Totalt"),
            mutate(., kstør_kom = "Totalt", landsdel_kom = "Totalt")) %>%  
  group_by(landsdel_kom, kstør_kom) %>% 
  summarize(svarprosent = round(ifelse(sum(valgt, na.rm = TRUE) == 0, 0, sum(godkjent, na.rm = TRUE)/sum(valgt, na.rm = TRUE)*100), digits = 1)) %>% 
  pivot_wider(names_from = kstør_kom, values_from = svarprosent) %>% 
  relocate("Totalt", .after = last_col()) %>% 
  relocate("Under 5000", .before = "5000 til 19 999") %>% 
  filter(!is.na(landsdel_kom)) %>%
  ungroup() %>% 
  slice(c(2, 5, 3, 1, 4))

#Utvalg, nye kategorier her også
tab2.17U <-  Hel_data %>% 
  filter(godkjent == "Godkjent") %>% 
  crosstable::crosstable(cols = landsdel_kom, by = kstør_kom, percent_pattern = list(body="{p_tot}", total_row = "{p_col}",
                                                                                     total_col ="{p_row}", total_all = "{p_col}"),
                         showNA = "no", percent_digits = 1, total = "both") %>% 
  rename("Under 5000 utvalg" = "Under 5000",
         "5000 til 19 999 utvalg" = "5000 til 19 999",
         "Mer enn 20 000 utvalg" = "Mer enn 20 000",
         "Totalt utvalg" = "Total") %>% 
  select(-.id, -label)


#Populasjon, nye kategorier her også
tab2.17P <-  Hel_data %>% 
  filter(kilder == "Pop_data") %>% 
  crosstable::crosstable(cols = landsdel, by = kstør_kom, percent_pattern = list(body="{p_tot}", total_row = "{p_col}",
                                                                                 total_col ="{p_row}", total_all = "{p_col}"),
                         showNA = "no", percent_digits = 1, total = "both") %>% 
  rename("Under 5000 populasjon" = "Under 5000",
         "5000 til 19 999 populasjon" = "5000 til 19 999",
         "Mer enn 20 000 populasjon" = "Mer enn 20 000",
         "Totalt populasjon" = "Total") %>% 
  select(-.id, -label)
#
tab2.17 <- inner_join(tab2.17P, tab2.17U, by = "variable") %>% 
  rename("Landsdel" = "variable") %>% 
  relocate(c("Landsdel", "Under 5000 utvalg", "Under 5000 populasjon", "5000 til 19 999 utvalg", "5000 til 19 999 populasjon",
             "Mer enn 20 000 utvalg", "Mer enn 20 000 populasjon", "Totalt utvalg", "Totalt populasjon"))

#Ritkig, bare feil rekkefølge på kolonner
tab2.18 <- Hel_data %>% 
  filter(kilder == "Utv_data",
         godkjent == "Godkjent") %>% 
  crosstable::crosstable(cols = s_1_1:s_1_5, by = sk4d, total = "both", percent_pattern = "{p_col}",
                         showNA = "no", percent_digits = 1) %>% 
  filter(variable == "Valgt") %>% 
  select(-.id, -variable)

#Kommuner
tab2.19k <- Hel_data %>% 
  filter(kilder == "Utv_data",
         godkjent == "Godkjent") %>% 
  crosstable::crosstable(cols = s_3_1:s_3_5, by = resp, percent_pattern = "{p_col}", total = "row", showNA = "no", percent_digits = 1) %>% 
  filter(variable == "Valgt") %>% 
  mutate(label = stringr::str_replace(label, "Rådmann, assisterende rådmann eller lignende", "\\(Fylkes\\)rådmann, assisterende rådmann og lignende"),
         label = stringr::str_replace(label, "Skolefaglig ansvarlig \\(eksempel utdanningsdirektør, skolesjef, oppvekstsjef, sek", "Skolefaglig ansvarlig \\(eksempel utdanningsdirektør, skolesjef, oppvekstsjef, seksjonssjef for skole\\)"),
         label = stringr::str_replace(label, "Seksjonsleder, avdelingsleder eller lignende stillinger på mellomledernivå", "Seksjonsleder, avdelingsleder og lignende stillinger på mellomledernivå"),
         label = stringr::str_replace(label, "Rådgiver, konsulent, førstesekretær eller lignende", "Rådgiver, konsulent, førstesekretær, og lignende"),
         label = stringr::str_replace(label, "Annen funksjon, spesifiser:", "Annet")) %>% 
  select(-.id) 

#Fylker, mutate må være etter crosstable. Fordi ellers finner den ikke "label".
tab2.19F <- Hel_data %>% 
  filter(kilder == "Utv_data",
         godkjent == "Godkjent") %>% 
  crosstable::crosstable(cols = s_5_1:s_5_5, by = resp, percent_pattern = "{n}", total = "col", showNA = "no", percent_digits = 1) %>% 
  filter(variable == "Valgt") %>% 
  mutate(label = stringr::str_replace(label, "Fylkesrådmann, assisterende rådmann eller lignende", "\\(Fylkes\\)rådmann, assisterende rådmann og lignende"),
         label = stringr::str_replace(label, "Skolefaglig ansvarlig \\(fylkesutdanningssjef, assisterende fylkesutdanningssjef e", "Skolefaglig ansvarlig \\(eksempel utdanningsdirektør, skolesjef, oppvekstsjef, seksjonssjef for skole\\)"),
         label = stringr::str_replace(label, "Seksjonsleder, avdelingsleder eller lignende stillinger på mellomledernivå", "Seksjonsleder, avdelingsleder og lignende stillinger på mellomledernivå"),
         label = stringr::str_replace(label, "Rådgiver, konsulent, førstesekretær eller lignende", "Rådgiver, konsulent, førstesekretær, og lignende"),
         label = stringr::str_replace(label, "Annen funksjon, spesifiser:", "Annet")) %>% 
  select(-.id)

tab2.19 <- inner_join(tab2.19k, tab2.19F, by = "label") %>% 
  select(-variable.x, -variable.y) %>% 
  relocate("Total", .after = last_col())



