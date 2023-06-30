#' Plot theme for NIFU graphs
#'
#' @param base_size set base text size
#' @param base_family set base font family
#' @param base_line_size set base line size
#' @param base_rect_size set base rectangle size
#' @param ... additional parameters passed to scale_*_nifu()
#'
#' @importFrom ggplot2 %+replace%
#' @export
#'
#' @examples
#' iris |> 
#'  ggplot(aes(Sepal.Length, Sepal.Width, color = Species)) + 
#'  geom_point() + 
#'  theme_nifu()
#' 
#' @returns styling for ggplot plot objects

theme_nifu <- function(base_size = 16,
                       base_family = "",
                       base_line_size = base_size / 22,
                       base_rect_size = base_size / 22,
                       ...){
  
  half_line <- base_size / 2
  
  t <- ggplot2::theme_grey(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    ggplot2::theme(
      axis.ticks      = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = ggplot2::rel(1.4)),
      axis.title.x = ggplot2::element_text(size = ggplot2::rel(1.8),
                                           margin = ggplot2::margin(10, 0, 0, 0),
                                           hjust = 1),
      axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.8),
                                           margin = ggplot2::margin(0, 10, 0, 0),
                                           angle = 90,
                                           hjust = 1),
      
      legend.background = ggplot2::element_blank(),
      legend.key        = ggplot2::element_blank(), 
      legend.text = ggplot2::element_text(size = ggplot2::rel(1.4)),
      legend.title = ggplot2::element_text(size = ggplot2::rel(1.4)),
      
      panel.background  = ggplot2::element_blank(),
      panel.border      = ggplot2::element_blank(),
      panel.grid = ggplot2::element_line(colour = "grey92"),
      panel.grid.minor = ggplot2::element_blank(),
      
      strip.background  = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = ggplot2::rel(1.4)),
      
      plot.background   = ggplot2::element_blank(),
      
      legend.position = "bottom",
      
      text = ggplot2::element_text(family = "Calibri"),
      
      complete = TRUE
    )
  
  
  list(
    t,
    scale_fill_nifu(...),
    scale_color_nifu(...),
    ggplot2::coord_cartesian(clip = "off")
  )
  
}