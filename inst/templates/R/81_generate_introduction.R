this_doc <- "R/generate_report/generate_introduction.R"
here::i_am(path = this_doc)
source(here::here("R", "generate_report", "configure_report_settings.R"))

list_words <- function(x, oxford=FALSE, divider=", ", last_divider=" og ") {
  y <- stringr::str_c(x[1:(length(x)-1)], collapse=divider)
  if(length(x)>1) stringr::str_c(c(y, last_divider, x[length(x)]), collapse="", sep="") else y
}

structure <-
  here::here(stringr::str_c(paths$file_structure_cur, collapse="/")) |> 
  readxl::read_excel(path = _, sheet=1) |> 
  dplyr::rename_with(.fn = ~stringr::str_replace(.x, " ", "_"))

used_cols <- 
  c("Kapittelnr", "Tema", "Fullt_forfatternavn", "project_manager", "Fullt_kvalitetssikrernavn", "GS", "KOM", "VGS", "FYL", "Udir_ansvarlig")
if(!all(used_cols %in% colnames(structure))) {
  cli::cli_abort("Din oversikt over kapitler mangler viktige kolonner: {.var {!used_cols %in% colnames(structure)}}")
}
if(nrow(structure)<2) {
  cli::cli_warn("Kapitteloversikten er svært liten. Noe galt?")
}
project_manager <- 
  structure |> 
  dplyr::filter(!is.na(project_manager)) |> 
  dplyr::distinct(project_manager) |> 
  dplyr::pull(project_manager)
if(length(project_manager)>1) cli::cli_warn("NB: Flere enn en project_manager står oppført. Riktig?")

coauthors <- 
  structure |>
  dplyr::filter(!is.na(Fullt_forfatternavn)) |> 
  dplyr::filter(!Fullt_forfatternavn %in% project_manager) |> 
  dplyr::distinct(Fullt_forfatternavn) |> 
  dplyr::pull(Fullt_forfatternavn) |> 
  list_words()

quality_assurers <- 
  structure |> 
  dplyr::filter(!is.na(Fullt_kvalitetssikrernavn)) |> 
  dplyr::distinct(Fullt_kvalitetssikrernavn) |> 
  dplyr::pull(Fullt_kvalitetssikrernavn) |> 
  list_words()

project_owner_managers <- 
  structure |> 
  dplyr::filter(!is.na(Udir_ansvarlig)) |> 
  dplyr::distinct(Udir_ansvarlig) |> 
  dplyr::pull(Udir_ansvarlig) |> 
  list_words()

n_topics <-
  structure |> 
  dplyr::filter(!is.na(Tema)) |> 
  dplyr::distinct(Tema) |> 
  nrow()

dat <- rio::import(here::here(stringr::str_c(paths$file_analysisdata_cur, collapse="/")))
n_gs <-
  dat |> 
  dplyr::filter(as.integer(resp) == 1) |> 
  nrow()
n_vgs <-
  dat |> 
  dplyr::filter(as.integer(resp) == 2) |> 
  nrow()
n_kom <-
  dat |> 
  dplyr::filter(as.integer(resp) == 3) |> 
  nrow()
n_fyl <-
  dat |> 
  dplyr::filter(as.integer(resp) == 4) |> 
  nrow()

topics_overview <-
  structure |> 
  dplyr::select(Kapittelnr, Tema, GS, VGS, KOM, FYL) |> 
  dplyr::mutate(dplyr::across(.fns = ~dplyr::if_else(is.na(.x), "", as.character(.x))))

################ Tidligere rapporter ################
n_reports <-
  here::here(paths$dir_pdf_all) |> 
  fs::dir_ls() |> 
  length()