nifu_pal <- function(palette = c("default"), alpha = 1){
  palette <- match.arg(palette)
  
  if(alpha > 1L | alpha <= 0L) stop("alpha must be in range (0, 1]")
  
  raw_cols <- c("nifu_red"    = rgb(200/255, 73/255, 87/255,
                                    alpha = alpha),
                "nifu_dark"   = rgb(64/255, 64/255, 64/255,
                                    alpha = alpha),
                "nifu_tan"    = rgb(237/255, 226/255, 210/255,
                                    alpha = alpha),
                "nifu_teal"   = rgb(45/255, 142/255, 159/255,
                                    alpha = alpha),
                "nifu_pink"   = rgb(219/255, 210/255, 224/255,
                                    alpha = alpha),
                "nifu_orange" = rgb(232/255, 174/255, 89/255,
                                    alpha = alpha))
  
  scales::manual_pal(unname(raw_cols))
}


scale_colour_nifu <- function(palette = c("default"), alpha = 1, ...){
  palette <- match.arg(palette)
  ggplot2::discrete_scale("colour", "nifu", nifu_pal(palette, alpha), ...)
}

scale_color_nifu <- scale_colour_nifu

scale_fill_nifu <- function(palette = c("default"), alpha = 1, ...) {
  palette <- match.arg(palette)
  discrete_scale("fill", "nifu", nifu_pal(palette, alpha), ...)
}