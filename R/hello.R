#'@title
#'make_poly
#'@description
#'Function to make a polygon out of 4 numbers representing the limits
#'@param limits 4 number in the order: x_min,y_min,x_max,y_max
#'@param crs_ the crs code
#'@returns
#'an sf polygon
#'@examples
#'
#'london_bb <- osmdata::getbb("London, UK", limit = 1) |> make_poly()
#'
#'
#'
#'@export
make_poly <- function(limits,crs_ = 4326){ # x_min,y_min,x_max,y_max,

  sf::st_polygon(x = list(
    matrix(data = c(limits[1],limits[2]
                    ,limits[1],limits[4]
                    ,limits[3],limits[4]
                    ,limits[3],limits[2]
                    ,limits[1],limits[2])
           #c(x_min,y_min, x_min,y_max, x_max,y_max, x_max,y_min, x_min,y_min)
           ,ncol = 2
           ,byrow = TRUE)
  )) |> sf::st_sfc(crs = crs_)
}

#'@title
#'factor_to_character
#'@description
#'Turn columns containing factors into character.
#'Solves the common problem when reading in files with character values that get turned into factor
#'@param dat a data frame that contains columns woth factors that we want to turn into character
#'
#'@returns the data frame where columns that had factors are turned into character
#'@examples
#' dt_fac <- data.frame(letters = factor(c("a","b","c"))
#'                  ,numbers = c(1,2,3))
#' class(dt_fac$letters)
#' dt_char <- factor_to_character(dt_fac)
#' class(dt_char$letters)
#'@export
factor_to_character <- function(dat) {
  dat <- dat |> as.data.frame()
  x <- sapply(dat, is.factor)
  dat[x] <- lapply(dat[x], as.character)
  dat
}



#'@title
#'get_lines
#'@description
#'This function is intended to turn sets of points into lines
#'If the to variable is null, then it is assumed that from
#'is a matrix where each row contains coordinates of a points
#'the line is then built by connecting each point in the order of the rows
#'@param from,to a matrix with 2 columns containing X and Y coordinates
#'@param crs integer number, the code of the reference system
#'@param by_element not used in current implementation
#'
#'@returns a sf object with linestrings
#@example
#'
#'
#'
#'
#'@export
get_lines <- function(from, to = NULL,crs = 4326, by_element = TRUE) {
  # check that from and to are points as well
  # this function is intended to turn points into lines
  # of the to variable is null, then it is assumed that from
  # is a matrix where each row contains coordinates of a points
  # the line is then built by connecting each point in the order of the rows

  if(!is.null(to)) {
    if( any(c("sf","sfc") %in% class(to)) & any(c("sf","sfc") %in% class(from))) {
      from <- from |>
        st_as_sf() |>
        st_coordinates()
      to <- to |>
        st_as_sf() |>
        st_coordinates()
    }

    return(
      apply(cbind(from,to)
            ,MARGIN = 1
            ,simplify = FALSE
            ,FUN = function(x) {
              matrix(data = x,ncol = 2, byrow = TRUE) |>
                st_linestring(dim = "XY")}) |>
        st_sfc(crs = crs)
    )

    # from_to <- cbind(from,to) |> t() |> data.table::as.data.table()

    # return(lapply(from_to,FUN = function(x) {
    #   matrix(data = x,ncol = 2, byrow = TRUE) |>
    #     st_linestring(dim = "XY")}) |>
    #     st_sfc(crs = crs))

  } else if (is.null(to)) {
    if("sf" %in% class(from)) {
      from <- from |>
        st_coordinates()
    }
    return(
      from[,1:2] |>
        as.matrix(ncol = 2, byrow = TRUE) |>
        st_linestring(dim = "XY") |>
        st_sfc(crs = crs)
    )
  }
}


#'@title
#'get_bb
#'@description
#'Function returning the bbox of a place from OpenStreetMap. It is a slightly more user friendly version
#' of the same function from the osmdata package.
#'@param area the name of an area to look for
#'@param format either 'rectangular' or 'polygon'
#'
#'@returns
#'The bbox of the place as either a rectangle or a more complex polygon shape
#'@examples
#'library(sf)
#' london_bb <- get_bb("London, UK")
#'@export
get_bb <- function(area, format = "rectangular"){
  if(format == "polygon"){
    osmdata::getbb(area,format_out = "sf_polygon",limit = 1) |> sf::st_geometry() |> sf::st_as_text()
  } else if (format == "rectangular"){
    osmdata::getbb(area, limit = 1) |> make_poly() |> sf::st_geometry() |> sf::st_as_text()
  } else {
    stop("provide the output format as either 'rectangular' or 'polygon'.")
  }

}


#########################
# HELP FUNCTIONS FOR SETS
#########################

#'@title overlap
#'@description
#'Helper function to for set comparisons. outputs either the fraction (DICE similarity) of
#'overlap in the sets. Or the number of common elements
#'@param x,y a set (vector) of elements
#'@param frac logical, TRUE to return the fraction of corresponding elements, FALSE to return the number
#'@returns either a values between 0 and 1 for the similarity of the sets
#'or an integer for the number of common elements
#'
#'@details
#'Uses the base function intersect and union, which consider only the unique elements of a set.
#'
#'@examples
#'x = c(1,2,3,4,5,55,5)
#'y = c(5,4,3,2,1,1,1,1)
#'overlap(x,y)
#'overlap(x,y,frac = FALSE)
#'
#'@export
overlap <- function(x,y, frac = TRUE){
  ifelse(frac
    ,return((base::intersect(x,y) |> length())/(base::union(x,y) |> length()))
    ,return(base::intersect(x,y) |> length())
    )
}


#'@title equals
#'@description
#'vectorized equal function to use in pipes. Useful for comparing values for summary statistics for example
#'@param x,y two elements to compare
#'
#'@returns a vector of logical values,
#' with each value corresponding to the comparison between the elements of the entered vectors.
#'@examples
#'x = c(1,2,3,4,5)
#'y = c(1,3,2,4,5)
#'
#'x |> equals(y)
#'
#'@export
equals <- function(x,y) {
  stopifnot(length(x) == length(y))
  return(x == y)
}



#'@title coord_to_text
#'@description  Convert two variables meant to contain x and y coordinates as numeric into wkt
#'@param  x,y numeric vectors
#'@returns a vector with character values containing wkt POINT (...).
#'@examples
#'
#'x = randu$x
#'y = randu$y
#'
#'xy_wkt = coord_to_text(x,y)
#'@export
coord_to_text <- function(x,y){
  stopifnot(length(x) == length(y))
  return(paste0("POINT (",x," ",y,")"))
}



