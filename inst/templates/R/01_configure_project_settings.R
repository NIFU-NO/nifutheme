################################################################################
### Global project settings across all SAROS-based reports and website
### Usually no need to touch the below if folder structure follows standard.
### Not to be run by itself. Will be run when running configure_report_settings.R
### NB: Possibly convert this into an easier-to-read _vars.yaml?
################################################################################

project_long <- "Spørringer til Skole-Norge"
project_short <- "Spørringene"

################################################################################
########## VARIABLE (COLUMN) NAMES USED ACROSS ALL REPORTS #####################
################################################################################

vars <- list() ### Initialize variable list here.
# Some variables may be constant across report cycles, then can be listed below.
# They will be merged with any  variables in a specific configure_report_settings.R
# if the outcome variables below are empty, will assume all non-predictor variables as outcome variables.
vars$outcome_binary <- c()
vars$outcome_ordinal <- c()
vars$outcome_nominal <- c()
vars$outcome_integer <- c()
########
vars$predictor_binary <- c()
vars$predictor_ordinal <- c()
vars$predictor_nominal <- c()
vars$predictor_interval <- c()



################################################################################
####################### Paths to folders and files #############################
################################################################################
paths <- list()

paths$dir_qmd_to_be_checked <- here::here("qmd_to_be_checked")

paths$dir_qmd_checked <- here::here("qmd_checked")
paths$dir_qmd_checked_main <- here::here(paths$dir_qmd_checked, "main")

paths$dir_data <- here::here("data")
paths$dir_pdf_all <- here::here(paths$dir_data, "pdf")
paths$dir_structure_all <- here::here(paths$dir_data, "kapitteloversikt")
paths$dir_questionnaire_all <- here::here(paths$dir_data, "spørreskjema")
paths$dir_surveydata_all <- here::here(paths$dir_data, "surveydata")
paths$dir_samplingframe_all <- here::here(paths$dir_data, "utvalg")
paths$dir_population_all <- here::here(paths$dir_data, "populasjon")

#### DO NOT CHANGE PATHS BELOW ####

paths$dir_images <- here::here("images")
paths$file_css <- here::here("_css/styles.scss")
paths$dir_site <- here::here("_site")
paths$saros_core <- here::here(Sys.getenv("USERPROFILE"), "NIFU", "Metode - General", "SAROS")
paths$renv_cache <- here::here(paths$saros_core, "renv_cache")
paths$dir_map <- here::here(paths$saros_core, "shared_data", "kart")
paths$dir_templates <- here::here(paths$saros_core, "shared_data", "maler")
paths$file_template_report <-
  here::here(paths$dir_templates, "Rapport_norsk_2020c.docx")
paths$file_template_workpaper <-
  here::here(paths$dir_templates, "Arbeidsnotat_norsk_2020d.docx")
#
# paths$dir_r_scripts <- "R"
# paths$dir_r_scripts_generate_report <-
#   paste0(paths$dir_r_scripts, "/generate_report")

