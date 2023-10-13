#' Check if a font file with the specified name is 
#' in the system's font directory
#'
#' @param font_name Name of font to be checked
#'
#' @return Boolean indicating font presence
#' @export
#'
#' @examples
#' is_font_installed("Comic Sans")

is_font_installed <- function(font_name) {
  if (Sys.info()["sysname"] == "Windows") {
    font_dir <- file.path(Sys.getenv("windir"), "Fonts")
  } else if (Sys.info()["sysname"] == "Darwin") {
    font_dir <- "/Library/Fonts"
  } else if (Sys.info()["sysname"] == "Linux") {
    font_dir <- "/usr/share/fonts"
  } else {
    stop("Unsupported operating system.")
  }
  
  system_fonts <- list.files(font_dir, 
                             pattern = ".ttf", 
                             full.names = TRUE)
  
  font_name <- tolower(font_name)
  installed <- any(grepl(font_name, 
                         tolower(system_fonts)))
  
  return(installed)
}