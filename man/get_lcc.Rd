% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cppr_support.R
\name{get_lcc}
\alias{get_lcc}
\title{get_lcc}
\usage{
get_lcc(ways, graph_mode = "weak")
}
\arguments{
\item{ways}{the edges as data frame or data.table}

\item{graph_mode}{the mode of the network to consider, weak by default.}
}
\value{
The edges data frame of the largest connected component
}
\description{
Return the largest connected component of a set of edges to further construct a cppr network.
}
\examples{

# make a small reprex
data("lille_graph")
get_lcc(ways=lille_graph$data)

}
