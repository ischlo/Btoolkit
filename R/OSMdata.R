#### Loading and manipulating OSM data with osmdata ####

#'@title getOSMdata
#'@name getOSMdata
#'@description
#'Facilitated function for working getting data from OSM with osmdata.
#'
#'@param bb a bbox
#'@param k OSM keys
#'@param val OSM values corresponding to the keys
#'@param timeout timeout for downloading. increase this value for bigger data sets.
#'@param memsize the maximum memory to dedicate to the process. increase this value for bigger data sets.
#'@param ... other parameters to the osmdata::opq function.
#'@returns
#'a list of lists containing the results from the overpass query in different spatial formats.
#'@examples
#'
#bb <- osmdata::getbb('marseille')
#'key <- 'amenity'
#'val <- 'place_of_worship'
#'
#res <- getOSMdata(bb=bb,k=key,val=val)
#'
#'@export
# facilitated osm query with osmdata
getOSMdata <- function(bb,k, val = "all",timeout = 600,memsize = 3073741824,...) {

  if (all(val != "all")) {
    osmdata::opq(bbox = bb,timeout = timeout,memsize = memsize,...) |>
      osmdata::add_osm_feature(key = k
                               ,value = val
                               ,key_exact = TRUE
                               ,value_exact = TRUE
                               ,match_case = TRUE) |>
      osmdata::osmdata_sf()
  } else {
    osmdata::opq(bbox = bb,timeout = timeout,memsize = memsize,...) |>
      osmdata::add_osm_feature(key = k
                               #,value = val
                               ,key_exact = TRUE
                               #,value_exact = TRUE
                               #,match_case = TRUE
      ) |>
      osmdata::osmdata_sf()
  }
}

#'@title osm_group_points
#'@name osm_group_points
#'@description
#'
#'Function for grouping the outputs of getOSMdata into a single spatial data frame of points. Useful for extracting polygonal or point like features. Polygons are transformed into their centroids.
#'
#'@param d a list of lists as returned by getOSMdata
#'@param k the keys of interest
#'@param kofinterest values of interest for the key k
#'@returns
#'a spatial data frame with points.
#'@examples
#'
#bb <- osmdata::getbb('marseille')
#'key <- 'amenity'
#'val <- 'place_of_worship'
#'
#res <- getOSMdata(bb=bb,k=key,val=val)
#'
#res <- osm_group_points(res,k='amenity',kofinterest='place_of_worship')
#'
#'@export
osm_group_points <- function(d,k,kofinterest = NULL) {

  if(is.null(kofinterest)) {
    df <- rbind(d$osm_points[sf::st_is_valid(d$osm_points$geometry),]
                ,d$osm_polygons[sf::st_is_valid(d$osm_polygons$geometry),] |> sf::st_centroid())
  } else {
    data_points <- d$osm_points[d$osm_points[[k]] %in% kofinterest,]
    data_poly <- d$osm_polygons[d$osm_polygons[[k]] %in% kofinterest,]

    df <- rbind(data_points[sf::st_is_valid(data_points$geometry),]
                ,data_poly[sf::st_is_valid(data_poly$geometry),] |> sf::st_centroid())
  }

  return(df[c("osm_id","name",k,"geometry")] |> `names<-`(c("osm_id","name","amenity","geometry")) |>
           sf::st_as_sf())
}

