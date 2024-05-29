#' london_msoa
#'
#' @format ## `london_msoa`
#' A data set with the msoa of London
#'
#' @source ONS, 2011
"london_msoa"



#'@name fdistance
#'@title fdistance
#'@description
#'Fast distance computations between points.
#'@param coord1,coord2 a matrix, data frame, data table containing at least two columns, with x and y coordinates.
#'if more than two columns are resent, only the first two are used. coord2 can be NULL.
#'@param one_to_one should the result be a matrix of all pair distances between the rows, or a distance vector of
#'one to one values. If one to one and both sets of coordinates are
#'@param projected boolean, depending on what type of coordinates are provided.
#'@param ... crs value when WKT is used
#'@details
#'Usually, unprojected coordinates are WGS84, all the others are projected.
#'For projected=FALSE the greater circle distance is used.
#'For projected, the euclidean distance.
#'@examples
#'library(sf)
#' data("london_msoa")
#'
#' samp1 <- sample(london_msoa$geometry,100) |>  st_transform(27700)|> st_centroid()|> st_coordinates()
#'
#' samp2 <- sample(london_msoa$geometry,100) |>  st_transform(27700)|> st_centroid()|> st_coordinates()
#'
#'
#' samp1_gc <- sample(london_msoa$geometry,100) |> st_centroid() |> st_coordinates()
#'
#' samp2_gc <- sample(london_msoa$geometry,100) |> st_centroid() |> st_coordinates()
#'
# samp_wkt <- sample(london_msoa$geometry,100) |> st_centroid() |> st_coordinates()
#'
#'dist_pair = fdistance(samp1,samp1,one_to_one = TRUE,projected = TRUE)
#'dist_pait_gc = fdistance(samp1,samp1,one_to_one = TRUE, projected=FALSE)
#'
#'dist_mat = fdistance(samp1,samp1,one_to_one = FALSE, projected=TRUE)
#'dist_mat_gc = fdistance(samp1,samp1,one_to_one = FALSE, projected=FALSE)
#'
#'@export
fdistance <- function(coord1,coord2 = NULL,projected=FALSE,one_to_one = FALSE,...) {

  # what we want from the input:
  # - if only coord1: return the dist matrix, so not one_to_one
  # - coods1 and coord2 need to be: at least two columns x and y, data.frame,data.table should be accepted or sf.

  stopifnot(inherits(coord1,c('matrix','array','data.table','data.frame','character'))
            ,any(is.null(coord2),inherits(coord2,c('matrix','array','data.table','data.frame','character')))
            ,any(is.null(coord2),!one_to_one,one_to_one & nrow(coord1)==nrow(coord2),one_to_one & length(coord1)==length(coord2))
            )

  if(inherits(coord1,'character') & !missing(...)) {

    # message("case 1")

    coord1 <- sf::st_as_sf(data.table::data.table(wkt=coord1)
                           ,...
                           # ,crs=4326
                           ,wkt=1) |> sf::st_coordinates()

  } else if (inherits(coord1,'character') & missing(...)) stop('provide crs for WKT in coord1.')

  if(inherits(coord2,'character') & !missing(...)) {

    # message("case 2")

    coord2 <- sf::st_as_sf(data.table::data.table(wkt=coord2)
                           ,...
                           # ,crs=4326
                           ,wkt=1) |> sf::st_coordinates()

  } else if (inherits(coord2,'character') & missing(...)) stop('provide crs for WKT in coord2.')


  if(inherits(coord1,c('sf',"sfc"))) {coord1 <- sf::st_geometry(coord1) |> sf::st_coordinates()}
  if(inherits(coord2,c('sf',"sfc"))) {coord1 <- sf::st_geometry(coord2) |> sf::st_coordinates()}

  if(inherits(coord1,c('data.frame','data.table'))) {coord1 <- as.matrix(coord1)}
  if(inherits(coord2,c('data.frame','data.table'))) {coord2 <- as.matrix(coord2)}

  if(inherits(coord1[[1]],'character')) {
    cli::cli_alert_info('detected WKT in coord1.\n')
    if(!missing(...)){
      coord1 <- sf::st_as_sf(data.table::data.table(wkt=coord1),...,wkt=1) |> sf::st_coordinates()
    }else{
      cli::cli_alert_warning('no crs provided, assuming 4326')
      coord1 <- sf::st_as_sf(data.table::data.table(wkt=coord1),crs=4326,wkt=1) |> sf::st_coordinates()
    }
  }
  if(inherits(coord2[[1]],'character')) {
    cli::cli_alert_info('detected WKT in coord2.\n')
    if(!missing(...)){
      coord2 <- sf::st_as_sf(data.table::data.table(wkt=coord2),...,wkt=1) |> sf::st_coordinates()
    }else{
      cli::cli_alert_warning('no crs provided, assuming 4326')
      coord2 <- sf::st_as_sf(data.table::data.table(wkt=coord2),crs=4326,wkt=1) |> sf::st_coordinates()
    }
  }

  if(all(one_to_one,!is.null(coord2),nrow(coord1)!=nrow(coord2))) { stop("Dimensions of the parameters don't match")}

  if (any(ncol(coord1) > 2, ncol(coord2) > 2)) {cli::cli_alert_info("Using the first two colunms as coordinates")}

  # coord1 <- as.matrix(coord1[,c(1,2)])

  # if(!is.null(coord2)) coord2 <- as.matrix(coord2[,c(1,2)])

  if(is.null(coord2) & !projected) {
    return(gc_distance_mat_cpp(coord1 = coord1,coord2 = coord1))
  }
  if(is.null(coord2) & projected) {
    return(distance_mat_cpp(coord1 = coord1,coord2 = coord1))
  }

  if(projected & one_to_one){
    return(distance_pair_cpp(coord1 = coord1,coord2 = coord2))
  }
  if(!projected & one_to_one){
    return(gc_distance_pair_cpp(coord1 = coord1,coord2 = coord2))
  }
  if(projected & !one_to_one){
    return(distance_mat_cpp(coord1 = coord1,coord2 = coord2))
  }
  if(!projected & !one_to_one){
    return(gc_distance_mat_cpp(coord1 = coord1,coord2 = coord2))
  }

}

#
#
# dat <- data.table::data.table(x = c(1,2,3,4,NA)
#                               ,y = c("a","b","c","d", NA))
# dat_1 <- matrix(data = c(1,2,3,4,NA,6,7,8),nrow = 4)
#
# any(is.na(dat))
#
# any(is.na(dat_1))
