




fast_distance <- function(x,y = NULL,one_to_one = FALSE, type = "euclid") {

  if(is_null(y)) {y <- x}

  if(length(intersect(class(x),c("sf","sfc")))!=0 & length(intersect(class(y),c("sf","sfc")))!=0){
    x <- st_coordinates(x)
    y <- st_coordinates(y)
  }

  if(length(x)==2 & length(y) > 2) {
    return(distance_cpp_vec(rep(x[1],nrow(y)),rep(x[2],nrow(y)),as.numeric(y[,1]),as.numeric(y[,2])))

  } else if (length(x) > 2 & length(y) == length(x)) {

    if (isTRUE(one_to_one)) {

      return(distance_cpp_vec(x[,1],x[,2],y[,1],y[,2]))

    } else if(!isTRUE(one_to_one)) {

      apply(x,MARGIN = 1,FUN= function(p) distance_cpp_vec(rep(p[1],nrow(y)),rep(p[2],nrow(y)),y[,1],y[,2])) %>% t()
    }
  } else {
    stop("provide the right data for x and y")
  }
}
