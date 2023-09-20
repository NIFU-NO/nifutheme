#Våren 2023 spørringene maps, the last Odyssey
library("tidyverse")
library("surveyreport")
library("haven")
library("readSX")
library("crosstable")
library("tibble")
library("sf")
library("rjson")
library("maps")
install.packages("maps")
# install and load the mapdata package
install.packages("mapdata")
library("mapdata")
install.packages("igraph")
library("igraph")



#Fylkekart fil
bakgrunnF <- "C:/Users/TorsteinDeBesche/NIFU/Metode - General/SAROS-core/shared resources/maps/fylker2021.json" %>% #sf::st_layers()
  read_sf(stringsAsFactors = TRUE, as_tibble = TRUE, layer = "fylker2021")

#Kommunekart fil
bakgrunn <- "C:/Users/TorsteinDeBesche/OneDrive - NIFU/R SAROS Torstein/SAROS Spørringene metode/kommuner2021.json" %>% #sf::st_layers()
  read_sf(stringsAsFactors = TRUE, as_tibble = TRUE, layer = "Kommuner2021") 


#Grunnskoler i hele Norge
Grunnskoler_populasjon <- read_dta ("C:/Users/TorsteinDeBesche/OneDrive - NIFU/R SAROS Torstein/SAROS Spørringene metode/gs_u.dta")

#Utvalg
utvalg_final <- read_dta ("C:/Users/TorsteinDeBesche/NIFU/21206 Utdanningsdirektoratets spørringer - Data - Data/Survey/V2023/ferdige filer/utvalg_final.dta")

Grunnskole_utvalg_final <- utvalg_final %>% #Svarprosent grunnskole (resp ==1, er grunnskoler) #prøver utvalg_final1 fordi dataset filen er rar, burde være samme utvalg_final.dta
  filter(resp == 1) %>% 
  filter(kommune != "Svalbard")  #tar ut svalbard (på grunn av kart)

Grunnskole_utvalg_final <- Grunnskole_utvalg_final %>%  #lager en ny variabel "ikke_godkjent" som er 0 for alle observasjoner
  mutate(Grunnskole_utvalg_final, ikke_godkjent = 0)


Grunnskole_utvalg_final <- Grunnskole_utvalg_final %>% #setter alle ikke_godkjent verdier til 1 når svarstatus > 2 (1 = godkjent, 2 = godkjent - noen svar), svarstatus finnes fra før
  mutate(ikke_godkjent = case_when(
    svarstatus > 2 ~ 1,
    TRUE ~ ikke_godkjent
  ))

Grunnskole_utvalg_final <- Grunnskole_utvalg_final %>% group_by(fylknr) %>% summarize(godkjent = sum(godkjent), ikke_godkjent = sum(ikke_godkjent))

Grunnskole_utvalg_final <- Grunnskole_utvalg_final %>% mutate(svarprosent = (godkjent / (godkjent + ikke_godkjent)) * 100)


Grunnskole_utvalg_final <- Grunnskole_utvalg_final %>% #putter svarprosent inn i kategorier
  mutate(cat_svarpr = case_when(
    svarprosent >= 0 & svarprosent < 10 ~ "0-9 prosent",
    svarprosent >= 10 & svarprosent < 20 ~ "10-19 prosent",
    svarprosent >= 20 & svarprosent < 30 ~ "20-29 prosent",
    svarprosent >= 30 & svarprosent < 40 ~ "30-39 prosent",
    svarprosent >= 40 & svarprosent < 50 ~ "40-49 prosent",
    svarprosent >= 50 & svarprosent < 60 ~ "50-59 prosent",
    svarprosent >= 60 & svarprosent < 70 ~ "60-69 prosent",
    svarprosent >= 70 & svarprosent < 80 ~ "70-79 prosent",
    svarprosent >= 80 & svarprosent < 90 ~ "80-89 prosent",
    svarprosent >= 90 & svarprosent <= 100 ~ "90-100 prosent", 
  ))

Grunnskoler_pop_utvalg <- left_join(Grunnskoler_populasjon, Grunnskole_utvalg_final, by = "fylknr")


Grunnskoler_pop_utvalg <- Grunnskoler_pop_utvalg %>% filter(!is.na(svarprosent.y))

Grunnskoler_pop_utvalg <- st_as_sf(Grunnskoler_pop_utvalg, coords = c("_CX", "_CY")) #Gjør om et objekt til sf og spesifiserer hvilke columns som er koordinater

st_crs(Grunnskoler_pop_utvalg) = 25833 #sf filen har ingen CRS kode enda, så vi må spesifisere det selv. 25833 er hentet fra https://epsg.io/25833 (Europe offshore and onshore). Nå vet R referansepunktet til koordinatene vi spesifiserte tidligere 

#Figur 2.1 grunnskoler
ggplot() +
  theme_void() +
  geom_sf(data = bakgrunn, fill = "white", alpha = .5) +
  geom_sf(data = Grunnskoler_pop_utvalg, color = "darkblue") #+
#scale_color_gradient( low = )

#Geografisk beliggenhet til vgs skoler som var med

VGS_utvalg_final <- utvalg_final %>%  #resp 2 = skoleleder vgs.
  filter(resp == 2) %>% 
  mutate(utvalgt = ifelse(!grepl("Longyearbyen", navn), 1, 0)) %>%  #utvalgt not to be confused with utvalg. Gir alle respondentene etter filtrering en verdi 1 på variabelen "utvalgt", tar også bort Longyearbyen på grunn av kart koordinater
  mutate(orgno = as.character(orgno))
#laster inn vgs koordinater kart

koordinaterVGS <- read_dta ("C:/Users/TorsteinDeBesche/OneDrive - NIFU/R SAROS Torstein/SAROS Spørringene metode/vgs.dta")

koordinaterVGS <- koordinaterVGS %>% 
  rename(orgno = organisasj) %>% 
  rename(navn = skolenavn)

koordinaterVGS <- koordinaterVGS %>% 
  mutate(orgno = as.character(orgno))

VGS_populasjon_utvalg <- left_join(VGS_utvalg_final, koordinaterVGS, by = "orgno")

VGS_populasjon_utvalg <- VGS_populasjon_utvalg %>% filter(!is.na(`_CX`)) #kan ikke ha missing variables når du spesifiserer coords med st_as_sf så tar dem bort


VGS_populasjon_utvalg <- st_as_sf(VGS_populasjon_utvalg, coords = c("_CX", "_CY"))

st_crs(VGS_populasjon_utvalg) = 25833 #se forklaring lenger oppe på st_crs(Grunnskoler_pop_utvalg)

#Figur 2.1 Videregående
ggplot() +
  theme_void() +
  geom_sf(data = bakgrunnF, fill = "white", alpha = .5) +
  geom_sf(data = VGS_populasjon_utvalg, color = "darkblue") 
# scale_color_gradient( low = )





kommuner_utvalg_final <- utvalg_final %>% #Stor K
  filter(resp == 3) %>% 
  rename(Kommunenummer = kmnr) %>% 
  rename(Kommunenavn = kommune) %>% 
  mutate(utvalgt = 1)

K_utvalgLjoin <- left_join(bakgrunn, kommuner_utvalg_final %>% select(Kommunenavn, utvalgt), by = c("Kommunenavn")) %>% 
  mutate(utvalgt = factor(ifelse(is.na(utvalgt), "white", "navyblue")))

#Figur 2.1, noen kommuner i Nordland som ikke er fyllt inn men men...
ggplot() +
  theme_void ()+
  geom_sf(data = K_utvalgLjoin,
          mapping = aes(fill = utvalgt)) +
  scale_fill_identity()


#Gjør om Fylkesnummer til numeric  
bakgrunnF <- bakgrunnF %>% mutate(Fylkesnummer = as.numeric(Fylkesnummer))

#Matcher Fylkesnummer så de stemmer overens med Fylkesnummer i SvarprosentGS3
bakgrunnF <- bakgrunnF %>% 
  mutate(Fylkesnummer = case_when(
    Fylkesnummer == 1 ~ 3,
    Fylkesnummer == 2 ~ 11,
    Fylkesnummer == 3 ~ 15,
    Fylkesnummer == 4 ~ 18,
    Fylkesnummer == 5 ~ 30,
    Fylkesnummer == 6 ~ 34,
    Fylkesnummer == 7 ~ 38,
    Fylkesnummer == 8 ~ 42,
    Fylkesnummer == 9 ~ 46,
    Fylkesnummer == 10 ~ 50,
    Fylkesnummer== 11 ~ 54,
    TRUE ~ NA_real_
  ))


#Renameer fylknr til Fylkesnummer så vi kan left_joine
Grunnskole_utvalg_final <- Grunnskole_utvalg_final %>% 
  rename(Fylkesnummer = fylknr)

Grunnskole_svarprosent <- left_join(bakgrunnF, Grunnskole_utvalg_final, by = "Fylkesnummer")


#Velger ut de to fylkene som er i nord; Nordland, og Troms og Finnmark, Figur 2.2 Nord
Grunnskole_svarprosentNord <- Grunnskole_svarprosent %>% 
  filter(Fylkesnummer == 54 | Fylkesnummer == 18) #%>% 
  #mutate(cat_svarpr = factor(ifelse(is.na(cat_svarpr), "white", "navyblue")))



#Figur 2.2 Nord
#Lager kart av Nord med svarprosent fylling
ggplot() +
  theme_void() +
  geom_sf(data = Grunnskole_svarprosentNord, 
          mapping = aes(fill = cat_svarpr)) #+
  #scale_fill_identity()
#+
#scale_fill_gradient(low = rgb(150/255, 183/255, 214/255), high = rgb(12/255, 60/255, 128/255)) 

#Error: Discrete value supplied to continuous scale, enten bytte ut cat_svarpr med svarprosent eller velg en annen måte å fylle inn farger på




#Velger ut bare de fylkene som er med i Sør kartet
Grunnskole_svarprosentSør <- Grunnskole_svarprosent %>% 
  filter(Fylkesnummer == 3 | Fylkesnummer == 11 |
           Fylkesnummer == 15 | Fylkesnummer == 30 |
           Fylkesnummer == 34 | Fylkesnummer == 38 |
           Fylkesnummer == 42 | Fylkesnummer == 46 |
           Fylkesnummer == 50)

#Figur 2.2 sør
#Lager kart av Sør og fyller svarprosent på grunnskoler per fylke.
ggplot() +
  theme_void() +
  geom_sf(data = Grunnskole_svarprosentSør, 
          mapping = aes(fill = cat_svarpr)) #Og hvordan legger jeg til antall grunnskoler i utvalget inni fylkene?





VGS_utvalg_final <- VGS_utvalg_final %>%  #lager en ny variabel "ikke_godkjent" som er 0 for alle observasjoner
  mutate(SvarprosentVGS, ikke_godkjent = 0) #%>% 


VGS_utvalg_final <- VGS_utvalg_final %>% #setter alle ikke_godkjent verdier til 1 når svarstatus > 2
  mutate(ikke_godkjent = case_when(
    svarstatus > 2 ~ 1,
    TRUE ~ ikke_godkjent
  ))


# Collapse the dataset by "fylknr", calculating the sum of "godkjent" and "ikke_godkjent" 
VGS_utvalg_final_groupedfylknr <- VGS_utvalg_final %>% group_by(fylknr) %>% summarize(godkjent = sum(godkjent), ikke_godkjent = sum(ikke_godkjent)) #%>% 
#select(c("fylknr", "fylke", "godkjent", "ikke_godkjent"))


# Create a new variable called "svarprosent" that represents the percentage of "godkjent" responses out of the total number of responses
VGS_utvalg_final_groupedfylknr <- VGS_utvalg_final_groupedfylknr %>% mutate(svarprosent = (godkjent / (godkjent + ikke_godkjent)) * 100) 

VGS_utvalg_final_groupedfylknr <- VGS_utvalg_final_groupedfylknr %>% #putter svarprosent inn i kategorier
  mutate (cat_svarpr = case_when(
    svarprosent >= 0 & svarprosent < 10 ~ "0-9 prosent",
    svarprosent >= 10 & svarprosent < 20 ~ "10-19 prosent",
    svarprosent >= 20 & svarprosent < 30 ~ "20-29 prosent",
    svarprosent >= 30 & svarprosent < 40 ~ "30-39 prosent",
    svarprosent >= 40 & svarprosent < 50 ~ "40-49 prosent",
    svarprosent >= 50 & svarprosent < 60 ~ "50-59 prosent",
    svarprosent >= 60 & svarprosent < 70 ~ "60-69 prosent",
    svarprosent >= 70 & svarprosent < 80 ~ "70-79 prosent",
    svarprosent >= 80 & svarprosent < 90 ~ "80-89 prosent",
    svarprosent >= 90 & svarprosent <= 100 ~ "90-100 prosent", 
    TRUE ~ NA_character_
  ))

#Renameer fylknr til Fylkesnummer så vi kan left_joine
VGS_utvalg_final_groupedfylknr <- VGS_utvalg_final_groupedfylknr %>% 
  rename(Fylkesnummer = fylknr)

VGS_svarprosent_populasjon_utvalg <- left_join(bakgrunnF, VGS_utvalg_final_groupedfylknr, by = "Fylkesnummer")

#Velger ut de to fylkene som er i nord; Nordland, og Troms og Finnmark
VGS_svarprosent_populasjon_utvalgNord <- VGS_svarprosent_populasjon_utvalg %>% #Ljoin = left_join
  filter(Fylkesnummer == 54 | Fylkesnummer == 18)



#Figur 2.3 Nord
#Lager kart av Nord med VGS svarprosent fylling.
ggplot() +
  theme_void() +
  geom_sf(data = VGS_svarprosent_populasjon_utvalgNord, 
          mapping = aes(fill = cat_svarpr))  




#Velger ut bare de fylkene som er med i Sør kartet
VGS_svarprosent_populasjon_utvalgSør <- VGS_svarprosent_populasjon_utvalg %>% 
  filter(Fylkesnummer == 3 | Fylkesnummer == 11 |
           Fylkesnummer == 15 | Fylkesnummer == 30 |
           Fylkesnummer == 34 | Fylkesnummer == 38 |
           Fylkesnummer == 42 | Fylkesnummer == 46 |
           Fylkesnummer == 50)

#Figur 2.3 Sør, riktig tall hvis du ser på tabell 2.11 i rapporten, men ikke riktig tall i forhold til figur 2.3 i rapporten. Figur 2.3 feil fargelagt?
#Lager kart av Sør og fyller svarprosent på VGS per fylke.
ggplot() +
  theme_void() +
  geom_sf(data = VGS_svarprosent_populasjon_utvalgSør, 
          mapping = aes(fill = cat_svarpr)) #Og hvordan legger jeg til antall grunnskoler i utvalget inni fylkene?







kommuner_utvalg_final <- kommuner_utvalg_final %>%  #Kopierer datasettet SvarprosentK og gir det nytt navn SvaprosentVGS1 og lager en ny variabel "ikke_godkjent" som er 0 for alle observasjoner
  mutate(SvarprosentK, ikke_godkjent = 0)  


kommuner_utvalg_final <- kommuner_utvalg_final %>% #setter alle ikke_godkjent verdier til 1 når svarstatus > 2
  mutate(ikke_godkjent = case_when(
    svarstatus > 2 ~ 1,
    TRUE ~ ikke_godkjent
  ))


# Collapse the dataset by "fylknr", calculating the sum of "godkjent" and "ikke_godkjent" #tallene stemmer litt, Oslo (3) stemmer ikke. Noe jeg har glemt?
kommuner_utvalg_final_groupedfylknr <- kommuner_utvalg_final %>% group_by(fylknr) %>% summarize(godkjent = sum(godkjent), ikke_godkjent = sum(ikke_godkjent))


# Create a new variable called "svarprosent" that represents the percentage of "godkjent" responses out of the total number of responses
kommuner_utvalg_final_groupedfylknr <- kommuner_utvalg_final_groupedfylknr %>% mutate(svarprosent = (godkjent / (godkjent + ikke_godkjent)) * 100)



kommuner_utvalg_final_groupedfylknr <- kommuner_utvalg_final_groupedfylknr %>% #putter svarprosent inn i kategorier
  mutate (cat_svarpr = case_when(
    svarprosent >= 0 & svarprosent < 10 ~ "0-9 prosent",
    svarprosent >= 10 & svarprosent < 20 ~ "10-19 prosent",
    svarprosent >= 20 & svarprosent < 30 ~ "20-29 prosent",
    svarprosent >= 30 & svarprosent < 40 ~ "30-39 prosent",
    svarprosent >= 40 & svarprosent < 50 ~ "40-49 prosent",
    svarprosent >= 50 & svarprosent < 60 ~ "50-59 prosent",
    svarprosent >= 60 & svarprosent < 70 ~ "60-69 prosent",
    svarprosent >= 70 & svarprosent < 80 ~ "70-79 prosent",
    svarprosent >= 80 & svarprosent < 90 ~ "80-89 prosent",
    svarprosent >= 90 & svarprosent <= 100 ~ "90-100 prosent", 
    TRUE ~ NA_character_
  ))

#Renameer fylknr til Fylkesnummer så vi kan left_joine
kommuner_utvalg_final_groupedfylknr <- kommuner_utvalg_final_groupedfylknr %>% 
  rename(Fylkesnummer = fylknr)


Kom_svarprosent_utvalg_populasjon <- left_join(bakgrunnF, kommuner_utvalg_final_groupedfylknr, by = "Fylkesnummer")

#Velger ut de to fylkene som er i nord; Nordland, og Troms og Finnmark
Kom_svarprosent_utvalg_populasjonNord <- Kom_svarprosent_utvalg_populasjon %>% 
  filter(Fylkesnummer == 54 | Fylkesnummer == 18)
#Figur 2.4 Nord
#Lager kart av Nord med Kommuner svarprosent fylling. Ingen tall som stemmer med Karl sine... Yikes.
ggplot() +
  theme_void() +
  geom_sf(data = Kom_svarprosent_utvalg_populasjonNord, 
          mapping = aes(fill = cat_svarpr))  




#Velger ut bare de fylkene som er med i Sør kartet
Kom_svarprosent_utvalg_populasjonSør <- Kom_svarprosent_utvalg_populasjon %>% 
  filter(Fylkesnummer == 3 | Fylkesnummer == 11 |
           Fylkesnummer == 15 | Fylkesnummer == 30 |
           Fylkesnummer == 34 | Fylkesnummer == 38 |
           Fylkesnummer == 42 | Fylkesnummer == 46 |
           Fylkesnummer == 50)

#Figur 2.4 Sør
#Lager kart av Sør og fyller svarprosent på kommuner per fylke.
#Måtte ta svarprosent fordi det er numeric, så nå har vi ikke de boksene på siden som kom opp når vi hadde cat_svarp i fill.
ggplot() +
  theme_void() +
  geom_sf(data = Kom_svarprosent_utvalg_populasjonSør, mapping = aes(fill = svarprosent)) +
  scale_fill_gradient(low = rgb(150/255, 183/255, 214/255), high = rgb(12/255, 60/255, 128/255))








