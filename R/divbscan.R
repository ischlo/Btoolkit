# divbscan

#'@title
#'entropy_iso
#'@description
#'Computes diversity measure for a set of categorical features contained whithin polygons provided in iso
#'@param d a data set of spatial features with a categorical variable
#'@param iso the accessibility areas for each location of interest, isodists for example.
#'@param by_ the variable containnig the category
#'@param cor_num how many cores to use for greater speed.
#'@returns a data frame with columns containing the various variables computed, including the entropy, the size, the ubiquity of the cluster
#@example
#'
#'
#'
#'
#'@export
entropy_iso <- function(d, iso, by_='amenity',cor_num = 2){

  # make sure that the isochrones data is perfectly alligned by row with the data.
  int <- sf::st_intersects(iso, d)
  # once we have made the intersection, we need to check that they all intersect at least with their own amenity
  # this is not the case every time because some amenities are in locations with no roads around
  # while the isochrones uses the underlyinig road network to build the areas.
  checks <- purrr::map(int,length) |> unlist()
  # when the intersection is zero, we impose that there is just the amenity for which the isochrone is computed
  bad_values <- which(checks == 0)
  # put the index of the amenity itself in the intersection
  int[bad_values] <- -1 #bad_values
  d <- data.table::as.data.table(d)

  entropy <- parallel::mclapply(int, mc.cores = cor_num, FUN = \(i) {
    if(all(i!=-1)){
      counted <- d[i,.N,by=by_]
      p <- counted$N/sum(counted$N)
      # ubiq <- nrow(counted)
      e <- c('entropy'=-sum(p*log(p))
             ,'size'=sum(counted$N)
             # ,'ubiq'=ubiq
             )
      return(e)
    } else {
      return(c('entropy'=NA,'size'=0
               # ,'ubiq'=0
               ))
    }
  })
  data.frame(matrix(entropy |> unlist(), ncol = 2, byrow = TRUE)) |> `colnames<-`(c('entropy','size'
                                                                                    # ,'ubiquity'
                                                                                    ))
}



neighbourhoods <- function(data,iso, cores = 1) {
  # pass data here in 27700 CRS
  # make sure that the isochrones data is perfectly alligned by row with the data.
  int <- sf::st_intersects(iso, data)
  # once we have made the intersection
  # , we need to check that they all intersect at least with their own amenity
  # this is not the case every time because some amenities are in locations with no roads around
  # while the isochrones uses the underlyinig road network to build the areas.
  checks <- map(int,length) |>unlist() |>tibble()
  # when the intersection is zero, we impose that
  # there is just the amenity for which the isochrone is computed
  bad_values <- which(checks[[1]] == 0)
  # put the index of the amenity itself in the intersectio
  int[bad_values] <- bad_values
  data$ind <- 1:nrow(data)

  max <- mcmapply(1:length(int),int, mc.cores = cores,FUN = \(i,val) {
    nb <- data[val,]
    indice <- i
    #osmid <- data$osm_id[i]
    m <- nb$ind[which(max(nb$entropy) == nb$entropy)[1]]
    it <- 0
    while(m != indice & (it<100)) {
      #nb <- data[int[[which(data$osm_id == m)]],]
      nb <- data[int[[m]],]
      indice <- m
      m <- nb$ind[which(max(nb$entropy) == nb$entropy)[1]]
      it <- it+1
    }
    m
  })
  max
}

divbscan <- list('entropy_iso'=entropy_iso
                 ,'neighbourhoods'=neighbourhoods)


usethis::use_data(divbscan,overwrite = TRUE)

#' divbscan list of functions.
#'
#'@format ## divbscan
#'This is an attempt to adapt a module approach to a set of functions in R, inspired by this feature in python.
#'This is a list object imported as data with the package and containing support functions for the divbscan algorithm.
#'
#'@source Ivann Schlosser, 2023
"divbscan"
