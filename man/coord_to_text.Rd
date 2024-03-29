% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hello.R
\name{coord_to_text}
\alias{coord_to_text}
\title{coord_to_text}
\usage{
coord_to_text(x, y)
}
\arguments{
\item{x, y}{numeric vectors}
}
\value{
a vector with character values containing wkt POINT (...).
}
\description{
Convert two variables meant to contain x and y coordinates as numeric into wkt POINTS
}
\examples{

x = randu$x
y = randu$y

xy_wkt = coord_to_text(x,y)
}
