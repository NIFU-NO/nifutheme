% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nifu_pal.R
\name{scale_fill_nifu}
\alias{scale_fill_nifu}
\title{NIFU fill scale}
\usage{
scale_fill_nifu(palette = "main", discrete = TRUE, reverse = FALSE, ...)
}
\arguments{
\item{palette}{Palette name}

\item{discrete}{Boolean indicating whether scale is discrete or not}

\item{reverse}{Boolean indicating direction of palette}

\item{...}{Ekstra parametre for ggplot2::discrete_scale() or
ggplot2::scale_colour_gradientn(), respectively}
}
\value{
ggproto object for fill aesthetics
}
\description{
NIFU fill scale
}
\examples{
library(ggplot2)
ggplot(iris, aes(Sepal.Length, Sepal.Width, fill = Species)) +
  geom_col() +
  scale_fill_nifu() 

}
