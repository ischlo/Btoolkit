#'@title
#'make_poly
#'@description
#'Function to make an sf polygon out of 4 values representing the boundaries
#'@param limits 4 number in the order: x_min,y_min,x_max,y_max
#'@param crs_ the crs code
#'@returns
#'an sf polygon
#'@examples
#'
#'london_bb <- osmdata::getbb("London, UK", limit = 1) |> make_poly()
#'
#'@export
make_poly <- function(limits,crs_ = 4326){ # x_min,y_min,x_max,y_max,

  if(limits[1]>limits[3]) stop('There might be errors in the order of limits.')
  if(limits[2]>limits[4]) stop('There might be errors in the order of limits.')

  if(!is.numeric(limits)) stop('PLese provide numeric values to limits')
  if(!is.numeric(crs_)) stop('Please provide a valid crs code')

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

  stopifnot(inherits(dat,'data.frame'))

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
#'
#'@returns a sf object with linestrings
#'@example
#'
#'from <- matrix(data=c(-0.13619,51.52203
#'                      ,-0.13064,51.52275)
#'               ,ncol=2
#'               ,by_row=TRUE)
#'
#'work_to_lunch <- get_lines(from=from)
#'tmap::tmap_mode('view)
#'tmap::qtm(work_to_lunch)
#'
#'@export
get_lines <- function(from, to = NULL,crs = 4326) {
  # check that from and to are points as well
  # this function is intended to turn points into lines
  # of the to variable is null, then it is assumed that from
  # is a matrix where each row contains coordinates of a points
  # the line is then built by connecting each point in the order of the rows

  stopifnot(is.numeric(crs))

  if(!is.null(to)) {
    if(any(c("sf","sfc") %in% class(to)) & any(c("sf","sfc") %in% class(from))) {

      from <- from |>
        sf::st_as_sf() |>
        sf::st_coordinates()
      to <- to |>
        sf::st_as_sf() |>
        sf::st_coordinates()
    }

    stopifnot(inherits(from,c('matrix','array'))
              ,inherits(to,c('matrix','array'))
              ,nrow(from)>=1
              ,nrow(to) >=1
              ,nrow(to)==nrow(from))

    return(
      apply(cbind(from,to)
            ,MARGIN = 1
            ,simplify = FALSE
            ,FUN = function(x) {
              matrix(data = x,ncol = 2, byrow = TRUE) |>
                sf::st_linestring(dim = "XY")}) |>
        sf::st_sfc(crs = crs)
    )

  } else if (is.null(to)) {
    if(any(c("sf","sfc") %in% class(from))) {
      from <- from |>
        sf::st_coordinates()
    }

    stopifnot(nrow(from)>=2
              ,inherits(from,c('matrix','array','data.frame')))

    return(
      from[,1:2] |>
        as.matrix() |>
        sf::st_linestring(dim = "XY") |>
        sf::st_sfc(crs = crs)
    )
  }
}


#'@title
#'get_bb
#'@description
#'Function returning the bbox of a place from OpenStreetMap. It is a slightly more user friendly version
#' of the same function from the osmdata package.
#' It is helpful when working with a remote postgis or other spatial db as it generates WKT output.
#'@param area the name of an area to look for
#'@param form either 'rectangular' or 'polygon'
#'
#'@returns
#'The bbox of the place as either a rectangle or a more complex polygon shape in text format
#'@examples
#'library(sf)
#'library(osmdata)
#'
#' london_bb <- get_bb("London, UK")
#'
#'@export
get_bb <- function(area, form = c("rectangular","polygon")){
  form <- match.arg(form)

  switch(form
         ,polygon=osmdata::getbb(area,format_out = "sf_polygon",limit = 1) |> sf::st_geometry() |> sf::st_as_text()
         ,rectangular=osmdata::getbb(area, limit = 1) |> make_poly() |> sf::st_geometry() |> sf::st_as_text()
  )
  #
  # if(form == "polygon"){
  #   osmdata::getbb(area,format_out = "sf_polygon",limit = 1) |> sf::st_geometry() |> sf::st_as_text()
  # } else if (form == "rectangular"){
  #   osmdata::getbb(area, limit = 1) |> make_poly() |> sf::st_geometry() |> sf::st_as_text()
  # } else {
  #   stop("provide the output format as either 'rectangular' or 'polygon'.")
  # }

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
#'
#'x = c(1,2,3,4,5)
#'y = c(1,3,2,4,5)
#'
#'x |> equals(y)
#'
#'@export
equals <- function(x,y) {
  stopifnot(length(x) == length(y)
            ,is.vector(x)
            ,is.vector(y))
  return(x == y)
}



#'@title coord_to_text
#'@description  Convert two variables meant to contain x and y coordinates as numeric into wkt POINTS
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
  stopifnot('both parameters must have the same length' = length(x) == length(y)
            ,'can not coerce to numeric' = !is.na(as.numeric(x))
            ,'can not coerce to numeric' = !is.na(as.numeric(y))
            )

  return(paste0("POINT (",x," ",y,")"))
}


#'@title samp_dt
#'@description  sample a data table or data frame. Can be used in pipes or on its own.
#'@param dt a data.table or data.frame
#'@param weight either a number less that 1 for a fraction of the dt,
#'or a positive integer for a specific number of rows
#'
#'@returns a sample of the data set
#'@examples
#'
#'data("cars")
#'
#'cars_half <- samp_dt(cars, .5)
#'cars_10 <- samp_dt(cars,10)
#'
#'@export
samp_dt <- function(dt, weight) {
  # data.table as the dt parameter
  # weight: either a number less that 1 for a fraction of the dt,
  # or a positive integer for a specific number of rows

  stopifnot(inherits(dt,c("data.frame",'data.table','tbl'))
            ,weight > 0
            ,weight<nrow(dt))

  if(weight < 1) {
    n <- floor(nrow(dt)*weight)
    return(dt[sample(1:nrow(dt),n),])
  } else if (weight >= 1) {
    return(dt[sample(1:nrow(dt),floor(weight)),])
  }
}


#'@title index_html
#'@description
#'Render a markdown file into a html file called index.html
#', for efficient creation of html pages directly publishable to github pages for example.
#'@example # rstudioapi::getSourceEditorContext()$path
#'@returns it is a void function that produces as output the html
#' file in the same directory in which the rmarkdown file is.
#'@export
index_html <- function() {
  cat(rstudioapi::getSourceEditorContext()$path)
  rmarkdown::render(input = rstudioapi::getSourceEditorContext()$path,output_file="index.html")
}


#'@title nlapply
#'@name nlapply
#'@description
#'This builds up on mapply and allows to access the name of the list element from within the function that you apply to the data.
#'Sometimes you might want to differentiate the operation that you do to the data based on the name of the
#'list element you apply this to and this function facilitates just that.
#'Simply provide a function in which the first parameter is supposed to be the name of the list element.
#'@param l a list, preferably named, otherwise it's the same as regular lapply.
#'@param fun a function of the type function(n,l) where n will be the name of the variable.
#'@param simplify passed to maaply, should results be simplified.
#'@param ... other arguments passed to lapply
#'@examples
#'# example code
#'my_list <- list('to_sum'=c(1,2,3,4,5)
#'                ,'to_multiply'=c(1,2,3,4,5))
#'
#'res <- nlapply(my_list
#'               ,fun=function(n,x){ if(n=='to_sum') sum(x)
#'                                   else if(n=='to_mult') prod(x)})
#'@export
nlapply <- function(l,fun,simplify=FALSE,...){
  if (is.null(names(l))) {
           warning('The list is not named, using regular lapply')
           return(parallel::mclapply(l,FUN = fun,...))
         }else {
           # print('Using named lapply')
           n <- names(l)
           return(parallel::mcmapply(n,l, FUN = fun,SIMPLIFY = simplify,...))
         }
}


#'@title as_geo
#'@name as_geo
#'@description
#'This function bridges the packages sf and data.table to build easier workflows using both.
#'It is meant to turn a data.table into a sf data.frame using a column that stores wkt in the data.table.
#'It is also designed to work whithin the j part of a data.table workflow
#', taking a character vector of wkt and turning it into a sf geometry column.
#'One especially relevant use case is when a data table has multiple columns that can potentially be used as geometries,
#'for example you might have polygons representing shapes, but also their centroids all in one table.
#'@details
#'If an sf object is passed as argument with the intent of turning a wkt column into geometry, then
#'the old geometry column is saved as wkt in a new column called `orig_geom`
#'This function proves relevant as well when interacting with a database that contains geometries.
#'They can be easily exported as WKT and then converted back in the environment if needed.
#'
#'@param dt a data.frame, data.table, tibble, sf data.frame ...
#'@param colname character/integer the column name or column number. If providing only a single column with the values to turn into geometries, just leave as is.
#'@param crs integer the crs to convert into
#'@param remove boolean. whether to remove or not the column that will be transformed into geometry.
#'@param ... Other optional arguments to pass to sf::st_as_sf
#'@returns a sf data.frame with the specified column used a geometry.
#'@examples
#' y <- rnorm(100,mean = 51,sd=1)
#' x <- rnorm(100)
#' p <- paste0('POINT ( ',x,' ',y,')')
#' dt <- data.table::data.table(id = seq_along(p),geom=p)
#' dt |> as_geo('geom')
#' dt[,'geom'] |> as_geo()
#' dt[,as_geo(geom)]
#'@export
as_geo <- function(dt,colname='geom_wkt', crs = 4326,remove=FALSE,...){

  if(inherits(dt,'character')){
    # message('provided a character vector, expected to contain WKT')
    return(tryCatch(dt |> data.table::as.data.table() |> sf::st_as_sf(wkt=1,crs=crs,remove=remove,...)
                    ,error=function(e) cli::cli_alert_danger('Failed, provide a vector containing wkt geometries.\n')
                    ,warning = function(w) cli::cli_alert_warning(w)))
  }

  # if the provided data set contains just one column, don't bother requiring colname.
  if(ncol(dt)==1) return(dt |> sf::st_as_sf(wkt=1,crs=crs,remove=remove,...))

  switch(is.character(colname)
         ,stopifnot('Provide a valid column name'=colname %in% colnames(dt))
         ,stopifnot('Provide a valid column number'=colname>=1L,colname<=ncol(dt))
         )

  if(is.character(colname)) {
    colname <- which(colnames(dt)==colname)
  }

  if(inherits(dt,c('sf'))) {
    col_sfc <-  sapply(dt,FUN=\(x) if('sfc' %in% class(x)) TRUE else FALSE,USE.NAMES = FALSE) |>
      which()

    if(col_sfc==colname) {
      # cat('Trivial\n')
      return(dt)
    }

    cli::cli_alert_info('storing the original geometry in the `orig_geom` column')
    dt$orig_geom <- dt[[col_sfc]] |> sf::st_as_text()

    return(dt |>
             sf::st_drop_geometry() |>
             sf::st_as_sf(wkt=colname,crs=crs,remove=remove,...)
    )

  }

  return(dt |> sf::st_as_sf(wkt=colname,crs=crs,remove=remove,...))

}
