% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/efficient_distance.R
\name{fdistance}
\alias{fdistance}
\title{fdistance}
\usage{
fdistance(coord1, coord2 = NULL, coords, one_to_one = TRUE)
}
\arguments{
\item{coord1, coord2}{a matrix, data frame, data table containing at least two columns, with x and y coordinates.
if more than two columns are resent, only the first two are used. coord2 can be NULL.}

\item{coords}{"projected" or "unprojected" , depending on what type of coordinates are provided.}

\item{one_to_one}{should the result be a matrix of all pair distances between the rows, or a distance vector of
one to one values. If one to one and both sets of coordinates are}
}
\description{
Fast distance computations between points.
}
\details{
Usually, unprojected coordinates are WGS84, all the others are projected.
For "unprojected" the greater circle distance is used.
For projected, the euclidean distance.
Currently, the function will fail if only one pair of coordinates is provided
this will be modified soon
}
\examples{
library(sf)
data("london_msoa")

samp1 <- sample(london_msoa$geometry,100) |>  st_transform(27700)|> st_centroid()|> st_coordinates()

samp2 <- sample(london_msoa$geometry,100) |>  st_transform(27700)|> st_centroid()|> st_coordinates()


samp1_gc <- sample(london_msoa$geometry,100) |> st_centroid() |> st_coordinates()

samp2_gc <- sample(london_msoa$geometry,100) |> st_centroid() |> st_coordinates()

dist_pair = fdistance(samp1,samp1,one_to_one = TRUE, coords = "projected")
dist_pait_gc = fdistance(samp1,samp1,one_to_one = TRUE, coords = "unprojected")

dist_mat = fdistance(samp1,samp1,one_to_one = FALSE, coords = "projected")
dist_mat_gc = fdistance(samp1,samp1,one_to_one = FALSE, coords = "unprojected")

}
