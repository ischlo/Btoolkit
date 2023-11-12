# This script will hold functions

cppr <- list()

cppr$make_lines <- function(paths, graph, crs = 4326) {

  col_names <- names(graph$coords)

  paths |> lapply(function(x) {graph$coords[match(x,col_names[1]),.(col_names[2],col_names[3])] |> # get the coordinates of the lines
      get_lines(to = NULL, crs = crs) # make the lines
  }) |>
    sf::st_sfc(crs = crs) |>
    sf::st_as_sf()
}


cppr$nearest_nodes <- function(graph, points_data = NULL,return_id = TRUE,...) {
  if(is.null(points_data)) stop("Provide sf spatial points to link to the network.")
  if(nrow(points_data)>1000) cat('Points data set is big, expect poor performance...')

  points_sf <- sf::st_as_sf(points_data,...)

  if(inherits(graph, c('data.table','data.frame'))){
    node_ind <- sf::st_nearest_feature(points_sf, graph |> sf::st_as_sf(coords=c('x','y'),crs = 4326))
    if(return_id) return(graph$osmid[node_ind])
    else return(node_ind)

  } else if (inherits(graph,'list')){
    node_ind <- sf::st_nearest_feature(points_sf, graph$coords |> sf::st_as_sf(coords=c('x','y'),crs = 4326))
    if(return_id) return(graph$coords$osmid[node_ind])
    else return(node_ind)
  }

}


cppr$get_lcc <- function(ways, graph_mode = "weak") {

  # require("igraph")

  stopifnot("data.table" %in% class(ways), "from" %in% colnames(ways), "to" %in% colnames(ways))

  igraph_ways <- igraph::graph_from_data_frame(ways[,.(from,to)],directed = FALSE)

  if(igraph_ways |> igraph::is_connected(mode = graph_mode)) {
    cat('Graph is connected')
    return(ways)
  }

  nodes_comp <- igraph::components(igraph_ways,mode = graph_mode)

  vert_ids <- igraph::V(igraph_ways)[nodes_comp$membership == which.max(nodes_comp$csize)]$name

  return(ways[from %in% vert_ids & to %in% vert_ids,])
}


cppr$make_network <- function(edges,nodes = NULL, simple = TRUE, directed = FALSE) {
  # Provide a edges and nodes from osmnx exported graph.
  # crs 4326 is expected
  # it's good if the data has also the variables from and to
  # designating nodes that are connected, but not essential
  edges <- edges |>
    as.data.table()
  try({nodes <- nodes |>
    as.data.table()})

  edges <- get_lcc(edges)

  graph <- edges[,.(from,to,length)]
  # we don't have enough info on the edges to make a directed graph, so the assumption
  # is taken that you can cycle in both direction on any edge
  graph <- graph |> cppRouting::makegraph(directed = directed
                                          ,coords = nodes[,.(osmid,x,y)])
  if(simple == TRUE) {
    # simplifying the graph
    graph <- graph |>
      cppRouting::cpp_simplify(rm_loop = TRUE
                               ,iterate = FALSE) # iterating may cause changes that we don't want
  }

  graph
}


cppr$add_edge <- function(graph
                          ,from
                          ,to
                          ,l) {
  # check if it does not already exist

  # find the codes of the node respectively
  from_id <- graph$dict[match(from,graph$dict$ref),"id"]
  to_id <- graph$dict[match(to,graph$dict$ref),"id"]

  stopifnot(is.numeric(from_id)
            ,is.numeric(to_id))

  graph$data <- rbind.data.frame(graph$data
                                 ,data.frame("from" = c(from_id,to_id)
                                             ,"to" = c(to_id,from_id)
                                             ,"dist"=c(l,l))) |> as.data.table()

  graph

}


cppr$plot_path <- function(graph, from_id, to_id){
  # require(tmap)
  p <- get_path_pair(graph
                     ,from = from_id
                     ,to = to_id
  )

  graph$coords[match(p[[1]],osmid),.(x,y)] |> get_lines() |> st_sfc(crs = 4326) |> tmap::qtm()
}

####

#'
#' cppr list of functions.
#'
#'@format ## cppr
#'This is an attempt to adapt a module approach to a set of functions in R, inspired by this feature in python.
#'This is a list object imported with the package and containing support functions for the cppr package.
#'
#'@source Ivann Schlosser, 2023
"cppr"

