
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

  paths |> lapply(function(x) {
    sel_nodes <- match(x,graph$coords[[1]])
    graph$coords[sel_nodes,c(2,3)] |> # get the coordinates of the lines
      get_lines(to = NULL, crs = crs) # make the lines
  }) |>
    unlist(recursive = FALSE) |>
    sf::st_sfc(crs = crs) |>
    sf::st_as_sf()
}


#' @title
#' nearest node
#' @description
#' Get the nearest network node to a point with lonlat coordinates.
#' @param graph the graph of interest
#' @param point the point of interest, as a sf object, or a vector with c(lat,lon) structure.
#' @param crs_ the crs of the data
#' @returns
#' The node with osmid,x,y variables.
#' @examples
#'
#' # make a small reprex
#' a <- 1
#'
#@export
nearest_nodes <- \(graph,point,crs_ = 4326){

    # res_ <- 12
    #
    # coords_ <- graph$coords
    # if(inherits(point,c('sf','sfc'))) point <- sf::st_coordinates(point)
    #
    # h3_cell <- h3::geo_to_h3(point,res=res_)
    #
    # h3_coords_ <- h3::geo_to_h3(coords_[,c(3,2)],res_)
    #
    # while(!(h3_cell %in% h3_coords_) & res_>1) {
    #   res_ <- res_-1
    #   print(paste0('resolution: ',res_))
    #   h3_cell <- h3::geo_to_h3(point,res=res_)
    #   h3_coords_ <- h3::geo_to_h3(coords_[,c(3,2)],res_)
    # }
    #
    # coords_ <- coords_[which(h3_coords_==h3_cell),]
    #
    # return(coords_[which.min(terra::distance(as.matrix(coords_[,.(x,y)]),sf::st_coordinates(point),lonlat=TRUE)),])

}


in_range <- \(r1,r2) {
  if ((r1[1] <= r2[2]) && (r1[1] >= r2[1]) && (r1[2] <= r2[2]) && (r1[2] >= r2[1])) TRUE
  else FALSE
}

#'@title
#'fnearest node
#'@description
#'Get the nearest network node to a point with projected coordinates extremely fast thanks to RANN
#'@param graph the graph of interest
#'@param pts the point(s) of interest, as a sf object, or a vector with c(lat,lon) structure.
#'@param nn how many nearest neighbour points to return for each pts.
#'@param local_crs if the coordinates are lonlat, provide a code for a local crs projection. Look up here: https://epsg.io/about
#'@param ... other parameters to RANN::nn2()
#'
#'@details
#'The main workflow expected is to have all your data in crs=4326
#'so when you use this function, you pass a local crs and the function converts automatically.
#'if no local crs is given, it is assumed that all the data is already projected.
#'It is difficult to verify if data is in the right crs when it's not an sf object,
#'so the precautions are mainly left to the user, with a minor test being done by the function.
#'
#'
#'@returns
#'The corresponding nearest nodes in graph to each pts.
#'@examples
#'
#'#Getting the graph for the french city of lille and a sample of 500 amenities
#'#from OSM provided with the package.
#' data("lille_amenities")
#' data("lille_graph")
#'
#' loc_crs <- 27561
#'
#' res <- cppr$fnearest_nodes(lille_graph,lille_amenities,local_crs = loc_crs)
#'
#' lille_graph$coords[res[[1]],]
#'
#'
#'@export
fnearest_nodes <- \(graph,pts,nn=1,local_crs = NULL,...){

  stopifnot(any(is.null(local_crs),is.numeric(local_crs))
            ,is.numeric(nn)
            ,nn>=1L
            ,local_crs>=1L
            )

  if(!is.null(local_crs)){

    stopifnot(sf::st_can_transform(4326,local_crs))

    if (!in_range(range(graph$coords[,2]),c(-180,180)) | !in_range(graph$coords[,3],c(-90,90))) stop('Coordinates of graph nodes not in WGS 84')

    node_coords <- graph$coords[,c(2,3)] |>
      sf::st_as_sf(coords=c(1,2),crs=4326) |>
      sf::st_transform(local_crs) |>
      sf::st_coordinates()

    #dealing with the points data
    if(inherits(pts,c('sf','sfc'))){
      if(sf::st_crs(pts)$input!="WGS 84") message('Non WGS 84 crs detected, converting to local_crs anyway')
      pts <- pts |>
        sf::st_transform(local_crs) |>
        sf::st_coordinates()

    } else if (inherits(pts,'matrix')) {
      stopifnot(ncol(pts)==2)
      pts <- pts |>
        as.data.frame() |>
        sf::st_as_sf(coords=c(1,2),crs=4326) |>
        sf::st_transform(local_crs) |>
        sf::st_coordinates()
    }

  } else {

    message('Assuming the data is in a projected crs')

    if (in_range(range(graph$coords[,2]),c(-180,180)) | in_range(graph$coords[,3],c(-90,90))) warning('Suspected lonlat provided without local_crs\n','Function might fail.')

    node_coords <- graph$coords[,c(2,3)] |>
      as.matrix()

    if(inherits(pts,c('sf','sfc'))){
      if(sf::st_crs(pts)$input=="WGS 84") stop('project your pts data or provide local_crs')
      pts <- pts |>
        sf::st_coordinates()
    } else if (inherits(pts,c('matrix','data.frame'))) {
      stopifnot(ncol(pts)==2)
      pts <- pts |>
        as.matrix()
    }
  }

  if(nrow(node_coords)>1e6) warning('graph very big, might cause failures')

  res <- RANN::nn2(data=node_coords,query = pts,k=nn,...)

  return(res)
}


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
#'data("lille_graph")
#'get_lcc(ways=lille_graph$data)
#'
#'@export
get_lcc <- function(ways, graph_mode = "weak") {

  # require("igraph")

  stopifnot(inherits(ways,'data.frame')
            ,all(c("from","to") %in% colnames(ways))
            )

  igraph_ways <- igraph::graph_from_data_frame(ways[,c('from','to')],directed = FALSE)

  if(igraph_ways |> igraph::is_connected(mode = graph_mode)) {
    cat('Graph is connected')
    return(ways)
  }

  nodes_comp <- igraph::components(igraph_ways,mode = graph_mode)

  vert_ids <- igraph::V(igraph_ways)[nodes_comp$membership == which.max(nodes_comp$csize)] |>
    attr('names')

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

  try({
    nodes <- nodes |>
    data.table::as.data.table()

    if(nrow(nodes)==0) {
      nodes <- NULL
    } else {
      nodes <- nodes[!duplicated(id),]
    }

    })

  edges <- get_lcc(edges[,c(1,2,3)])

  graph <- edges[,c(1,2,3)]

  # we don't have enough info on the edges to make a directed graph, so the assumption
  # is taken that you can cycle in both direction on any edge

  graph <- cppRouting::makegraph(df=graph
                                 ,directed = directed
                                 ,coords = nodes
  )

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

  graph$coords[match(p[[1]],osmid),c('x','y')] |>
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

cppr$fnearest_nodes <- fnearest_nodes

cppr$get_lcc <- get_lcc

usethis::use_data(cppr,overwrite = TRUE)

#'
#' cppr list of functions.
#'
#'@format ## cppr
#'This is an attempt to adapt a module approach to a set of functions in R, inspired by this feature in python.
#'This is a list object imported with the package and containing support functions for the cppr package.
#'
#'@source Ivann Schlosser, 2023
"cppr"



#'
#' cppRouting graph for examples.
#'
#'@format ## lille_graph
#' A graph constructed from OSM data with the cppRouting package.
#'
#'@source Ivann Schlosser, 2023
"lille_graph"



#'
#' lille amenities
#'
#'@format ## lille_amenities
#'A sample of OSM amenities from the city of Lille
#'
#'@source Ivann Schlosser, 2023
"lille_amenities"


#'
#'
#'
#'@format ## nodes
#' A data frame of nodes to be used with the data set of segments to test the cppr related functions
#'
#'@source Ivann Schlosser, 2023
"nodes"


#'
#'
#'
#'@format ## segments
#' A data frame of segments to be used with the data set of nodes to test the cppr related functions
#'
#'@source Ivann Schlosser, 2023
"segments"

