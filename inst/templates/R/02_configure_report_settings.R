################################################################################
######################## Report-specific settings ##############################
################################################################################
here::i_am(path = "")
source(here::here("configure_project_settings.R"))

## Innhent årgang fra mappenavnet til der hvor qmd_filene og data ligger?
cycle <- "2022H" # Alltid på formatet åååå-semesterbokstav. Dersom spesialundersøkelse, legg til f.eks. -L for lærer, etc

cycle_pretty <- # Trekke ut til en egen excel-fil eller eget generate_report skript?
  stringr::str_replace(cycle, 
                     "([[:digit:]]{4})([[:alpha:]]{1})",
                     "\\2\\1") |> 
  stringr::str_replace_all(pattern = "^[[:alpha:]]", 
                           replacement = function(.x) {
                             dplyr::case_when(.x=="H" ~ "høsten ", 
                                                           .x=="V" ~ "våren ",
                                                           .x=="S" ~ "sommeren ",
                                                           .x=="L" ~ "til lærere i ",
                                                           .x=="X" ~ "ekstrasurvey ",
                                                           TRUE ~ .x)
                             })

################################################################################
########## VARIABLE (COLUMN) NAMES USED IN THIS REPORT #########################
################################################################################

# Some variables may vary across report cycles, they can be listed below.
vars$predictor_binary = c(vars$predictor_binary)
vars$predictor_ordinal = c(vars$predictor_ordinal)
vars$predictor_nominal = c(vars$predictor_nominal)
vars$predictor_interval = c(vars$predictor_interval)

# if the outcome variables below are empty, will assume all non-predictor variables as outcome variables.
vars$outcome_binary <- c(vars$outcome_binary)
vars$outcome_ordinal <- c(vars$outcome_ordinal)
vars$outcome_nominal <- c(vars$outcome_nominal)
vars$outcome_integer <- c(vars$outcome_integer)

################################################################################
################ Rapportspesifikke mappe- og filbaner ##########################
################################################################################
stopifnot(exists("paths"))
paths$dir_surveydata_all <- c(paths$dir_data, "surveydata")
paths$dir_samplingframe_all <- c(paths$dir_data, "utvalg")
paths$dir_population_all <- c(paths$dir_data, "populasjon")

paths$file_structure_cur <- 
  c(paths$dir_structure_all, cycle, paste0(cycle, "_kapitteloversikt.xlsx"))

paths$file_questionnaire_cur <- 
  c(paths$dir_questionnaire_all, cycle, paste0(cycle, "_spørreskjema.pdf"))

paths$file_surveydata_cur <- 
  c(paths$dir_surveydata_all, cycle, paste0(cycle, "_qualtricsdata.json"))

paths$file_samplingframe_cur <- 
  c(paths$dir_samplingframe_all, cycle, paste0(cycle, "_utvalg.parquet"))

paths$file_methoddata_cur <- 
  c(paths$dir_methoddata_all, cycle, "metodedata.parquet")

paths$file_analysisdata_cur <- 
  c(paths$dir_analysisdata_all, cycle, "analyseklar.parquet")

## Unnecessary steps. Only if dataset has been prepared in Stata
# paths$file_data_ready_dta_cur <- 
#   c(paths$dir_data, "survey", cycle, "ferdige filer", "arbeidsfil_final.dta")
# paths$file_data_labels_cur <- 
#   c(paths$dir_data, "survey", cycle, "arbeidsfiler", "labes_r.xls")



## For data preparation and response rate in method chapter
# paths$file_data_popframe_cur <- 
#   c(paths$dir_data, cycle, "endelig utvalg")



