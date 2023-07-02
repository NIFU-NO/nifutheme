#' Specify NIFU colours
#'
#' @param type Type of palette, defaulting to a discrete colour palette. 
#'              Possible types are c("main", "reds", "blues")
#' @param ... Additional arguments passed to ggplot2
#'
#' @export nifu_cols
#'
#' @examples
#' nifu_cols()
#' 
#' @returns vector of hex codes for use in colour and fill aesthetics

nifu_cols <- function(type = "main", ...){
  
  if(!type %in% c("main", "reds", "blues")){
    stop(paste0("The palette '", type, 
               "' is not available for this function. Please choose one of 
               c('main', 'reds', 'blues') as the type argument."))
  }
  
  if(type == "main"){
    cols_used <- c(
      "#C84957",
      "#404040",
      "#EDE2D2",
      "#E8AE59",
      "#2D8E9F",
      "#DBD2E0")
  } else if (type == "reds"){
    cols_used <- c(
      "#E8B0B7",
      "#DE919A",
      "#D5727D",
      "#C84957",
      "#BC3848",
      "#9D2F3C",
      "#7E2630")
  } else if (type == "blues"){
    cols_used <- c(
      "#90D4E0",
      "#70C7D7",
      "#50BBCE",
      "#36AABF",
      "#2D8E9F",
      "#24727F",
      "#1B555F")
  }
  
  cols <- c(...)
  
  if(is.null(cols))
    return (cols_used)
  
  cols_used[cols]
}

#' Create a color palette in NIFU colours
#'
#' @param palette Type of palette
#' @param reverse Specify direction of colour palette
#' @param ... Additional arguments to colorRampPalette
#' 
#' @importFrom grDevices colorRampPalette
#'
nifu_pal <- function(palette = "main",
                     reverse = FALSE,
                     ...){
  
  pal <- nifu_cols(palette)
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
  
}


#' NIFU colour scale
#'
#' @param palette Palette name
#' @param discrete Boolean indicating whether scale is discrete or not
#' @param reverse Boolean indicating direction of palette
#' @param ... Ekstra parametre for ggplot2::discrete_scale() or 
#'            ggplot2::scale_colour_gradientn(), respectively
#' 
#' @importFrom ggplot2 discrete_scale
#' @export scale_colour_nifu
#'
#' @examples 
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, fill = Species)) +
#'   geom_point() +
#'   scale_color_nifu() 
#' 
#'
#' @returns ggproto object for color aesthetic
#'
scale_colour_nifu <- function(palette = "main", 
                              discrete = TRUE, 
                              reverse = FALSE,
                              ...){
  
  if(palette == "main" & !discrete){
    warning("You have specified a non-discrete colour palette yet named a discrete palette. 
            Consider providing one of c('reds', 'blues') to the 'palette' argument for better
            results for continuous variables")
  }
  
  pal <- nifu_pal(palette = palette, reverse = reverse)
  
  if(discrete){
    ggplot2::discrete_scale(aesthetics = "colour", 
                            scale_name = paste0("nifu_", palette), 
                            palette = pal, 
                            ...)
  } else {
    ggplot2::scale_colour_gradientn(colours = pal(256) ,...)
  }
  
}

#' Alias for scale_colour_nifu()
#' 
#' @inheritParams scale_colour_nifu
#' @export scale_color_nifu
#' @returns ggproto object for colour aesthetic
#'
scale_color_nifu <- scale_colour_nifu



#' NIFU fill scale
#' 
#' @inheritParams scale_colour_nifu
#' @export scale_fill_nifu
#' @importFrom ggplot2 discrete_scale
#' 
#' @returns ggproto object for fill aesthetics
#' 
#' @examples
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, fill = Species)) +
#'   geom_col() +
#'   scale_fill_nifu() 
#' 
scale_fill_nifu <- function(palette = "main", 
                              discrete = TRUE, 
                              reverse = FALSE,
                              ...){
  
  if(palette == "main" & !discrete){
    warning("You have specified a non-discrete fill palette yet named a discrete palette. 
            Consider providing one of c('reds', 'blues') to the 'palette' argument for better
            results for continuous variables")
  }
  
  pal <- nifu_pal(palette = palette, reverse = reverse)
  
  if(discrete){
    ggplot2::discrete_scale(aesthetics = "fill", 
                            scale_name = paste0("nifu_", palette), 
                            palette = pal, 
                            ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256) ,...)
  }
  
}