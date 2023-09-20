################################################################################
############                                                     ###############
############ Prepare raw data (identifiable ID variables).       ###############
############                                                     ###############
################################################################################
# Try to leave it as generic as possible to ease re-usability across projects 
# although this is hard. No statistical analyses here!



### Data obtained from Spørringene til Skole-Norge survey
### Internal path to folder with meta-information (questionnaire form,
### sources of instruments, application form for retrieving data, etc.) about the data:
### NA

dat_sporringene <- # To obtain stratification variables, municipality size, etc.
  import(file = paths$dat_sporringer, setclass = "tbl") #%>%
  # select(gsiid, arsverk, sk5d, sk4d, landsdel,        # Which columns to keep
  #        gs3d, målform, orgform, folketall)

## View data, either by clicking the symbol next to the data-set in the
## environment list on the right, or using syntax (not capital V):
view(dat_sporringene)

### Check variable names and labels (latter if applicable)
names(dat_sporringene)
dat_sporringene %>%
  lookfor(details = FALSE) %>%
  view()

### Sort variables (temporarily, unless we assign back to dat_sporringene)
dat_sporringene %>%
  arrange(landsdel, desc(folketall)) %>%
  view()


### Data obtained from (URL): https://gsi.udir.no
### Internal path to folder with meta-information (questionnaire form,
### sources of instruments, application form for retrieving data, etc.) about the data:
### NA

dat_gsi <- # To retrieve street addresses and number of students per school
  import(file = paths$dat_gsi, setclass = "tbl",
             skip = 6,             # Skip first 6 lines (see for yourself in Excel-file). We also ignore the existing column names (line 6)
             col_names = c("school", "year", "gsiid",
                           "visit_address", "postal_address",
                           "postal_number", "postal_place",
                           "postal_number2", "postal_place2",
                           "municipality_number", "municipality",
                           "fylke_number", "fylke",
                           "skole_type", "driftsansvar",
                           "n_trinn5_20_21", "n_trinn6_20_21", "n_trinn7_20_21"),
             trim_ws = TRUE) %>%
  filter(if_any(.cols = matches("n_trinn"), .fns =  ~.x > 0)) %>% # Keep rows if at least some students on grades 5, 6 OR 7.
  mutate(gsiid = as.integer(gsiid))                               # Convert gsiid from character to integer

dat_gsi %>% # Control that there are no/few duplicates
  select(school, municipality) %>%
  filter(duplicated(.))
# This last school we could perhaps fix manually.




### Data obtained from (URL): https://www.survey-xact.no/analysis?analysisid=1787152
### Internal path to folder with meta-information (questionnaire form,
### sources of instruments, application form for retrieving data, etc.) about the data:
### 2_Forberedelser_datainnsamling/04 Spørreundersøkelse/Målgruppe1/1-Hovedgjennomføring #1/
### Survey to teachers in elementary school 2020

dat_survey_raw <-
  read_surveyxact(
    filepath = paths$dat_survey_raw,
    remove_whitespace = TRUE) %>%
  filter(!skole %in% c("a", "NTNU")) %>%                                        # Keep only if school is not "a" or "NTNU"
  mutate(skole = case_when(skole == "Hegra skole" ~ "Hegra barneskole",
                           skole == "Utvorda oppvekstsenter" ~ "Utvorda skole",
                           skole == "Rosmælen skole" ~ "Rosmælen skole og barnehage",
                           skole == "Bud barne- og ungdomsskole" ~ "Bud barne- og ungdomsskule",
                           skole == "Skjelstadmark skole" ~ "Skjelstadmark oppvekstsenter skole",
                           TRUE ~ skole),
         resp_status = case_when(statoverall_1==1 ~ "1-Not invited",
                                 statoverall_2==1 ~ "2-Not responded",
                                 statoverall_3==1 ~ "3-Partial response",
                                 statoverall_4==1 ~ "4-Completed",
                                 statoverall_5==1 ~ "5-Excused (unknown reason)",
                                 skole %in% c("a", "NTNU") ~ "9-Not in population"))

val_labels(dat_survey_raw)
# Some labels look weird - because we added page breaks in SurveyXact value labels

val_labels(dat_survey_raw) <-                                                   # Replace existing value labels for all columns
  val_labels(dat_survey_raw) %>%                                                # Take all the value labels for all columns..
  map(.,                                                                        # For each column's value labels
      ~if(length(.x)>0) set_names(.x, gsub("\\r|\\n", " ", names(.x)))) %>%     # If labels exist, replace line shifts/page breaks with space in labels (names(.x)) and return the entire vector.
  map(.,                                                                        # For each column's value labels
      ~if(length(.x)>0) set_names(.x, gsub("[[:space:]]{2,}", " ", names(.x)))) # If labels exist, replace double spaces with single space


#########################################################################################################
#### Join dat_gsi and dat_school_survey to dat_survey_raw, dropping schools not present in the latter. #
#########################################################################################################

## First check how well the datasets match each other. Should only be 4 Swedish schools not matching.
anti_join(x = dat_survey_raw, y = dat_gsi, by=c("skole" = "school")) %>%        # return all rows from dat_survey_raw without a match in dat_gsi.
  distinct(skole) %>%
  summarize(n_distinct(skole, na.rm = T))                                       # All but 4 Swedish schools match


dat_survey <-
  dat_survey_raw %>%
  left_join(x = ., y = dat_gsi, by = c("skole" = "school")) %>%                 # Merges what can be found in dat_gsi to dat_survey_raw, keeping all rows in dat_survey_raw
  left_join(x = ., y = dat_sporringene, by = "gsiid") %>%
  mutate(visit_address =
           case_when(visit_address == "Bruhagen" ~ "Djupmyrveien 21",           # Fix some errors I discovered during the process
                     visit_address == "Sveberg" ~ "Svebergvegen 4",
                     TRUE ~ visit_address),                                     # Otherwise, use visit_address
         visit_number = as.integer(gsub("[^0-9/]*", "", visit_address)),        # Gives warnings, but they are ok.
         visit_street = gsub("([[:alpha:][:space:]]*) [0-9/]*$", "\\1", visit_address), # Extract anything before a final number
         visit_street = gsub("[0-9\\-]*", "", visit_street),                    # Remove any leftover numbers or dashes
         visit_street = trimws(visit_street),                                   # Remove whitespace from beginning and end of strings
         URL =
           glue(.na = "",
         "https://ws.geonorge.no/adresser/v1/sok?kommunenummer={municipality_number}&adressenavn={visit_street}&nummer={visit_number}&filtrer=adresser.representasjonspunkt",
         ),
         URL = gsub("nummer=&", "", URL),
         URL = as.character(URL),
         URL = gsub(" ", "%20", URL))

############ Join geographical location information for schools ################
### The following is a slightly complicated setup to lookup addresses and return coordinates.

tmp <-                                                                          # Try not to run this many times, as it will bother geonorge.no...
  dat_survey %>%
  filter(!is.na(visit_address)) %>%                                             # Remove empty addresses
  distinct(URL) %>%                                                             # Remove duplicate URLs
  pull(URL) %>%                                                                 # Take out URL column from dataset and make it a vector
  set_names() %>%                                                               # The vector will get names, being the same as itself
  map_dfr(., .id = "URL",
      .f =  function(.x) {                                  # For each of these URLs, do the following, and return a tibble (dataframe)
    try(silent = TRUE, expr = {                                                 # If it fails to lookup, continue without complaining
      print(.x)                                                                 # Print the URL to us (just so we can see what goes on)
      x <-
        read_html(x = .x) %>%                                                   # Visit the URL and read the HTML into R
        xml_text()                                                             # Take out the text from the HTML code
      lon <-
        str_extract(string = x, pattern = 'lon\":[0-9\\.]*') %>%                # Extract longitude from the text (+ some junk)
        str_remove(string = ., pattern = '.*:') %>%                             # Remove junk
        as.numeric()                                                            # Convert from character to numeric
      lat <-
        str_extract(string = x, pattern = 'lat\":[0-9\\.]*') %>%
        str_remove(string = ., pattern = '.*:') %>%
        as.numeric()

      tibble(long=lon, lat=lat)                                                 # Make a tibble (dataframe) with column names long and lat

    })})                                                                        # Return the tibble and continue the loop

## Check non-matching addresses. Can be added manually - if we care.
tmp %>%
  filter(is.na(long) | is.na(lat))

dat_survey <-
  dat_survey %>%
  select(!one_of("long", "lat")) %>% # If columns long and lat are already in dat_survey, removes them. In case one runs the same command twice.
  left_join(x = ., y = tmp, by = "URL")

export(x = dat_survey_raw, file = paste0(paths$dat, "dat_survey_raw.xlsx"))
export(x = dat_survey, file = paste0(paths$dat, "dat_survey.xlsx"))

