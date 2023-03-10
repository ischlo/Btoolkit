% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hello.R
\name{get_bb}
\alias{get_bb}
\title{get_bb}
\usage{
get_bb(area, format = "rectangular")
}
\arguments{
\item{area}{the name of an area to look for}

\item{format}{either 'rectangular' or 'polygon'}
}
\value{
The bbox of the place as either a rectangle or a more complex polygon shape
}
\description{
Function returning the bbox of a place from OpenStreetMap. It is a slightly more user friendly version
of the same function from the osmdata package.
}
\examples{
library(sf)
london_bb <- get_bb("London, UK")
}
