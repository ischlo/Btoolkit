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
#'@param coords "projected" or "unprojected" , depending on what type of coordinates are provided.
#'
#'@details
#'Usually, unprojected coordinates are WGS84, all the others are projected.
#'For "unprojected" the greater circle distance is used.
#'For projected, the euclidean distance.
#'Currently, the function will fail if only one pair of coordinates is provided
#'this will be modified soon
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
#'dist_pair = fdistance(samp1,samp1,one_to_one = TRUE, coords = "projected")
#'dist_pait_gc = fdistance(samp1,samp1,one_to_one = TRUE, coords = "unprojected")
#'
#'dist_mat = fdistance(samp1,samp1,one_to_one = FALSE, coords = "projected")
#'dist_mat_gc = fdistance(samp1,samp1,one_to_one = FALSE, coords = "unprojected")
#'
#'@export
fdistance <- function(coord1,coord2 = NULL,coords,one_to_one = TRUE) {

  if (all(class(coord1) == "numeric",length(coord1) == 2)) { coord1 <<- matrix(coord1,ncol = 2,byrow = TRUE)}
  if (all(class(coord2) == "numeric",length(coord2) == 2)) { coord2 <<- matrix(coord2,ncol = 2,byrow = TRUE)}

  stopifnot(is.numeric(unlist(coord1))
            ,any(is.null(coord2),is.numeric(unlist(coord2)))
            ,ncol(coord1) >= 2
            ,any(is.null(coord2),ncol(coord2) >= 2)
            ,coords %in% c("projected", "unprojected")
            ,all(!is.na(coord1))
            ,!is.null(coord2) & all(!is.na(coord2))
            )

  if(one_to_one & !is.null(coord2) & nrow(coord1)!=nrow(coord2)) { stop("Dimensions of the parameters don't match")}

  coord1 <- as.matrix(coord1[,c(1,2)])


  if(!is.null(coord2)){ coord2 <<- as.matrix(coord2[,c(1,2)])}

  if (any(ncol(coord1) > 2, ncol(coord2) > 2)) {print("Using the first two colunms as coordinates")}

  if(one_to_one & is.null(coord2)) {
    switch (coords,
            "projected" = distance_pair_cpp(coord1 = coord1,coord2 = coord1)
            ,"unprojected" = gc_distance_pair_cpp(coord1 = coord1,coord2 = coord1)
    )
  } else if (one_to_one & !is.null(coord2)){
    switch (coords,
            "projected" = distance_pair_cpp(coord1 = coord1,coord2 = coord2)
            ,"unprojected" = gc_distance_pair_cpp(coord1 = coord1,coord2 = coord2)
    )
  } else if(!one_to_one & is.null(coord2)) {
    switch (coords,
            "projected" = distance_mat_cpp(coord1 = coord1,coord2 = coord1)
            ,"unprojected" = gc_distance_mat_cpp(coord1 = coord1,coord2 = coord1)
    )
  } else if (!one_to_one & !is.null(coord2)) {
    switch (coords,
            "projected" = distance_mat_cpp(coord1 = coord1,coord2 = coord2)
            ,"unprojected" = gc_distance_mat_cpp(coord1 = coord1,coord2 = coord2)
    )
  }
}




dat <- data.table::data.table(x = c(1,2,3,4,NA)
                              ,y = c("a","b","c","d", NA))
dat_1 <- matrix(data = c(1,2,3,4,NA,6,7,8),nrow = 4)

any(is.na(dat))

any(is.na(dat_1))
