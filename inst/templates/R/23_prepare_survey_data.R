################################################################################
##################### Import Qualtrics survey data and prepare #################
################################################################################

survey_name <- paste0(project_short, "_", cycle)

# Get surveydata
dat_survey <- 
  qualtRics::fetch_survey(surveyID = all_surveys()$id[survey_name], 
                          verbose = TRUE)

dat_questionnaire <-
  qualtRics::survey_questions(surveyID = all_surveys()$id[survey_name])

dat_questionnaire2 <-
  qualtRics::extract_colmap(dat_survey)

dat_survey_metadata <- 
  qualtRics::metadata(surveyID = all_surveys()$id[survey_name], 
                      get = c("metadata", "questions", "responsecounts", "blocks", "flow", "embedded_data", "comments"))


survey_date_opened <- "?. august 2022" #Innhente fra metadata fra Qualtrics, og omgjøre til norsk langdato. Flytte til eget generate_report skript?
survey_date_closed <- "21. oktober 2022"
