# divbscan

divbscan <- list()

divbscan$entropy_iso <- function(d, iso, cor_num = 1){
  # make sure that the isochrones data is perfectly alligned by row with the data.
  int <- sf::st_intersects(iso, d)
  # once we have made the intersection, we need to check that they all intersect at least with their own amenity
  # this is not the case every time because some amenities are in locations with no roads around
  # while the isochrones uses the underlyinig road network to build the areas.
  checks <- map(int,length) |> unlist() |> tibble()
  # when the intersection is zero, we impose that there is just the amenity for which the isochrone is computed
  bad_values <- which(checks[[1]] == 0)
  # put the index of the amenity itself in the intersectio
  int[bad_values] <- bad_values

  # registerDoParallel(cor_num)
  # entropy <- foreach(i = 1:nrow(d), .combine=c) %dopar% {
  #   counted <- d[int[[i]],] |> dplyr::group_by(amenity) |> dplyr::count()
  #   p <- counted$n/sum(counted$n)
  #   e <- c(-sum(p*log(p)),sum(counted$n))
  # }
  d <- data.table::as.data.table(d)

  entropy <- mclapply(int, mc.cores = cor_num, FUN = \(i) {
    counted <- d[i,.N,by='amenity']
    p <- counted$N/sum(counted$N)
    e <- c(-sum(p*log(p)),sum(counted$N))
    e
  })
  entropy
  # stopImplicitCluster()
  data.frame(matrix(entropy |> unlist(), ncol = 2, byrow = TRUE)) |> rename("entropy" = "X1", "size" = "X2")
}



divbscan$neighbourhood <- function(data,iso, cores = 1) {
  # pass data here in 27700 CRS
  # make sure that the isochrones data is perfectly alligned by row with the data.
  int <- sf::st_intersects(iso, data)
  # once we have made the intersection, we need to check that they all intersect at least with their own amenity
  # this is not the case every time because some amenities are in locations with no roads around
  # while the isochrones uses the underlyinig road network to build the areas.
  checks <- map(int,length) |>unlist() |>tibble()
  # when the intersection is zero, we impose that there is just the amenity for which the isochrone is computed
  bad_values <- which(checks[[1]] == 0)
  # put the index of the amenity itself in the intersectio
  int[bad_values] <- bad_values
  data$ind <- 1:nrow(data)
  # registerDoParallel(cores)
  # max <- foreach(i = 1:nrow(data), .combine = c) %dopar% {
  #   nb <- data[int[[i]],]
  #   indice <- i
  #   #osmid <- data$osm_id[i]
  #   m <- nb$ind[which(max(nb$entropy) == nb$entropy)[1]]
  #   while(m != indice) {
  #     #nb <- data[int[[which(data$osm_id == m)]],]
  #     nb <- data[int[[m]],]
  #     indice <- m
  #     m <- nb$ind[which(max(nb$entropy) == nb$entropy)[1]]
  #   }
  #   m
  # }
  # stopImplicitCluster()

  max <- mcmapply(1:length(int),int, mc.cores = cores,FUN = \(i,val) {
    nb <- data[val,]
    indice <- i
    #osmid <- data$osm_id[i]
    m <- nb$ind[which(max(nb$entropy) == nb$entropy)[1]]
    while(m != indice) {
      #nb <- data[int[[which(data$osm_id == m)]],]
      nb <- data[int[[m]],]
      indice <- m
      m <- nb$ind[which(max(nb$entropy) == nb$entropy)[1]]
    }
    m
  })


  max
}


#' divbscan list of functions.
#'
#'@format ## divbscan
#'This is an attempt to adapt a module approach to a set of functions in R, inspired by this feature in python.
#'This is a list object imported as data with the package and containing support functions for the divbscan algorithm.
#'
#'@source Ivann Schlosser, 2023
"divbscan"








