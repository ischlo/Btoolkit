
#'@title
#'make_lines
#'@description
#'Make an sf linesting data frame out of a set of paths as returned by the cppRouting get_path_pair, get_multi_paths
#'@param paths a list of nodes constituting paths returned by get_path_pair, get_multi_paths
#'@param graph the graph used.
#'@param crs the crs of the data
#'@returns
#'an sf linestring data frame
#'@examples
#'
#' # make a small reprex
#'a <- 1
#'
#'@export
make_lines <- function(paths, graph, crs = 4326) {

  col_titles <- colnames(graph$coords)

  paths |> lapply(function(x) {graph$coords[match(x,col_titles[1]),.(col_titles[2],col_titles[3])] |> # get the coordinates of the lines
      get_lines(to = NULL, crs = crs) # make the lines
  }) |>
    sf::st_sfc(crs = crs) |>
    sf::st_as_sf()
}


#'@title
#'nearest node
#'@description
#'Get the nearest network node to a point with lonlat coordinates.
#'@param graph the graph of interest
#'@param point the point of interest, as a sf object, or a vector with c(lat,lon) structure.
#'@param crs_ the crs of the data
#'@returns
#'The node with osmid,x,y variables.
#'@examples
#'
#' # make a small reprex
#'a <- 1
#'
#'@export
nearest_nodes <- \(graph,point,crs_ = 4326){

    res_ <- 12

    coords_ <- graph$coords
    if(inherits(point,c('sf','sfc'))) point <- sf::st_coordinates(point)

    h3_cell <- h3::geo_to_h3(point,res=res_)

    h3_coords_ <- h3::geo_to_h3(coords_[,c(3,2)],res_)

    while(!(h3_cell %in% h3_coords_) & res_>1) {
      res_ <- res_-1
      print(paste0('resolution: ',res_))
      h3_cell <- h3::geo_to_h3(point,res=res_)
      h3_coords_ <- h3::geo_to_h3(coords_[,c(3,2)],res_)
    }

    coords_ <- coords_[which(h3_coords_==h3_cell),]

    return(coords_[which.min(terra::distance(as.matrix(coords_[,.(x,y)]),sf::st_coordinates(point),lonlat=TRUE)),])

  }

# function(graph, points_data = NULL,return_id = TRUE,...) {
#   if(is.null(points_data)) stop("Provide sf spatial points to link to the network.")
#   if(nrow(points_data)>1000) cat('Points data set is big, expect poor performance...')
#
#   points_sf <- sf::st_as_sf(points_data,...)
#
#   if(inherits(graph, c('data.table','data.frame'))){
#     node_ind <- sf::st_nearest_feature(points_sf, graph |> sf::st_as_sf(coords=c('x','y'),crs = 4326))
#     if(return_id) return(graph$osmid[node_ind])
#     else return(node_ind)
#
#   } else if (inherits(graph,'list')){
#     node_ind <- sf::st_nearest_feature(points_sf, graph$coords |> sf::st_as_sf(coords=c('x','y'),crs = 4326))
#     if(return_id) return(graph$coords$osmid[node_ind])
#     else return(node_ind)
#   }
#
# }


#'@title
#'get_lcc
#'@description
#'Return the largest connected component of a set of edges to further construct a cppr network.
#'@param ways the edges as data frame or data.table
#'@param graph_mode the mode of the network to consider, weak by default.
#'@returns
#'The edges data frame of the largest connected component
#'@examples
#'
#' # make a small reprex
#'a <- 1
#'
#'@export
get_lcc <- function(ways, graph_mode = "weak") {

  # require("igraph")

  stopifnot("data.table" %in% class(ways), "from" %in% colnames(ways), "to" %in% coltitles(ways))

  igraph_ways <- igraph::graph_from_data_frame(ways[,.(from,to)],directed = FALSE)

  if(igraph_ways |> igraph::is_connected(mode = graph_mode)) {
    cat('Graph is connected')
    return(ways)
  }

  nodes_comp <- igraph::components(igraph_ways,mode = graph_mode)

  vert_ids <- igraph::V(igraph_ways)[nodes_comp$membership == which.max(nodes_comp$csize)]$title

  return(ways[from %in% vert_ids & to %in% vert_ids,])
}


#'@title
#'make_network
#'@description
#'From a data frame of edges and nodes construct a cppRouting graph.
#'@param edges the edges with at least the columns from,to,length.
#'@param nodes the nodes with columns id,x,y. Optionnal.
#'@param simple simplify the network, default TRUE
#'@param directed is the network directed, default FALSE
#'@returns
#'The edges data frame of the largest connected component
#'@examples
#'
#' # make a small reprex
#'a <- 1
#'
#'@export
make_network <- function(edges,nodes = NULL, simple = TRUE, directed = FALSE) {
  # Provide a edges and nodes from osmnx exported graph.
  # crs 4326 is expected
  # it's good if the data has also the variables from and to
  # designating nodes that are connected, but not essential
  edges <- edges |>
    data.table::as.data.table()
  try({nodes <- nodes |>
    data.table::as.data.table()})

  edges <- get_lcc(edges)

  graph <- edges[,.(from,to,length)]
  # we don't have enough info on the edges to make a directed graph, so the assumption
  # is taken that you can cycle in both direction on any edge
  graph <- graph |> cppRouting::makegraph(directed = directed
                                          ,coords = nodes[,.(osmid,x,y)])
  if(simple) {
    # simplifying the graph
    graph <- graph |>
      cppRouting::cpp_simplify(rm_loop = TRUE
                               ,iterate = FALSE) # iterating may cause changes that we don't want
  }

  graph
}

#'@title
#'add_edge
#'@description
#'Add an edge to a cppRouting network
#'@param graph a graph constructed with the cppRouting package.
#'@param from the node from which to start the edge, has to be in the original nodes.
#'@param to the end node of the edge
#'@param l the length of the edge.
#'
#'@returns
#'the updated graph
#'@examples
#'
#' # make a small reprex
#'a <- 1
#'
#'@export
add_edge <- function(graph
                     ,from
                     ,to
                     ,l) {
  # check if it does not already exist
  # make the l parameter optionnal and if not provided take the euclid distance between the nodes.
  # make the case for directed networks
  # find the codes of the node respectively
  from_id <- graph$dict[match(from,graph$dict$ref),"id"]
  to_id <- graph$dict[match(to,graph$dict$ref),"id"]

  stopifnot(is.numeric(from_id)
            ,is.numeric(to_id))

  graph$data <- rbind.data.frame(graph$data
                                 ,data.frame("from" = c(from_id,to_id)
                                             ,"to" = c(to_id,from_id)
                                             ,"dist"=c(l,l))) |> data.table::as.data.table()

  graph

}

#'@title
#'plot_path
#'@description
#'plot a path between two nodes. Useful for debugging and quick visualisations.
#'@param graph a graph constructed with the cppRouting package.
#'@param from_id the node from which to start the path, has to be in the original nodes.
#'@param to_id the end node of the path
#'
#'@returns
#'plots the path with tmap
#'@examples
#'
#' # make a small reprex
#'a <- 1
#'
#'@export
plot_path <- function(graph, from_id, to_id){
  # require(tmap)
  p <- cppRouting::get_path_pair(graph
                                 ,from = from_id
                                 ,to = to_id
  )

  graph$coords[match(p[[1]],osmid),.(x,y)] |>
    get_lines() |>
    sf::st_sfc(crs = 4326) |>
    leaflet::leaflet() |>
    leaflet::addTiles() |>
    leaflet::addPolylines(fillColor = 'darkblue'
                          ,fillOpacity = 1
                          ,opacity = .5
                          ,color = 'lightblue')
}

####

cppr <- list()

cppr$make_lines <- make_lines

cppr$plot_path <- plot_path

cppr$add_edge <- add_edge

cppr$make_network <- make_network

cppr$nearest_nodes <- nearest_nodes

cppr$get_lcc <- get_lcc

#'
#' cppr list of functions.
#'
#'@format ## cppr
#'This is an attempt to adapt a module approach to a set of functions in R, inspired by this feature in python.
#'This is a list object imported with the package and containing support functions for the cppr package.
#'
#'@source Ivann Schlosser, 2023
"cppr"

