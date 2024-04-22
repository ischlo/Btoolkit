test_that("make_poly", {
  # check the arguments
  expect_error(make_poly(c(3,2,1,4)),'There might be errors in the order of limits.')
  expect_error(make_poly(c(1,4,3,2)),'There might be errors in the order of limits.')
  #check a simple input
  polygon_output <- sf::st_sfc(sf::st_point(c(1,2))
                                           ,sf::st_point(c(1,4))
                                           ,sf::st_point(c(3,4))
                                           ,sf::st_point(c(3,2))
                                           ,sf::st_point(c(1,2))
                                           ,crs=4326) |>
    sf::st_combine() |>
    sf::st_cast('POLYGON')

  make_poly_output <- make_poly(c(1,2,3,4))

  expect_equal(make_poly_output,make_poly_output)

})


test_that('get_lines',{

  get_lines_input <- matrix(data = c(1,1,2,2,3,3,4,4),ncol = 2,byrow = TRUE)

  get_lines_output <- get_lines_input |> sf::st_linestring(dim = 'XY') |> sf::st_sfc(crs=4326)

  # expected success cases

  expect_equal(get_lines(get_lines_input),get_lines_output)

  expect_s3_class(class = 'sfc',
                  object=get_lines(from=get_lines_input[-4,]
                                   ,to=get_lines_input[-1,])
  )

  expect_s3_class(class = 'sfc'
                  ,object = get_lines(from=get_lines_input)
                  )

  #between two points only.
  expect_s3_class(class = 'sfc'
                  ,object=get_lines(from=get_lines_input[1,] |> matrix(ncol=2)
                                    ,to=get_lines_input[3,] |> matrix(ncol=2)))

  ## Expected error cases

  # crs is bad
  expect_error(get_lines(from = get_lines_input,crs = 'svjn'))

  #different sizes when from and to are provided
  expect_error(get_lines(from=get_lines_input
                         ,to=get_lines_input[-1,]))

  #only a single line value is provided.
  expect_error(get_lines(from=get_lines_input[1,]))

  #between two points that are not provided as matrix
  expect_error(get_lines(from=get_lines_input[1,]
                         ,to=get_lines_input[3,]))


})


skip_if_offline()
test_that('get_bb',{

  ## success case

  expect_type(object = get_bb('London,UK')
              ,type = 'character')

  expect_match(object = get_bb('London,UK')
               ,regexp = '^POLYGON')

  ### ERROR cases

  expect_error(get_bb('blablaland'))

})



test_that('equals',{

  x <- runif(100)

  y <- runif(101)

  expect_length(equals(x,y[-1]),n = length(x))

  expect_type(object=equals(x,y[-1]),type='logical')

  expect_identical(equals(x,y[-1]),x==y[-1])

  #### ERRORS

  expect_error(equals(x,y))

  expect_error(equals(data.frame(x),data.frame(y[-1])))

})


test_that('named lapply',{
  my_list <- list('to_sum'=c(1,2,3,4,5)
                  ,'to_multiply'=c(1,2,3,4,5))

  res <- nlapply(my_list
                 ,fun=function(n,x){ if(n=='to_sum') sum(x)
                   else if(n=='to_multiply') prod(x)})

  expect_type(object=nlapply(my_list
                        ,fun=function(n,x){ if(n=='to_sum') sum(x)
                          else if(n=='to_multiply') prod(x)})
              ,type = 'list')


  ### ERRORS/Exceptions

  expect_warning(nlapply(my_list |> unname()
                         ,fun=function(x) sum(x)))


})


test_that('coord_to_text',{

  data(quakes)

  quakes$geom_wkt <- coord_to_text(quakes$long,quakes$lat)
  quakes$geom_bla <- 'blabla'

  # success
  expect_type(type = 'character'
              ,coord_to_text(quakes$long,quakes$lat))

  # errors
  expect_error(
    coord_to_text(quakes$geom_bla,quakes$lat)
    )

})


test_that('as_geo',{

  data(quakes)

  quakes$geom_wkt <- coord_to_text(quakes$long,quakes$lat)
  quakes$geom_bla <- 'blabla'

  quakes_sf <- quakes |>
    sf::st_as_sf(coords=c(2,1),crs=4326)

  # success

  #passing a character vector with valid WKT
  expect_s3_class(as_geo(quakes$geom_wkt)
                  ,class = 'sf')

  # passing a data.frame with a valid WKT character column name
  expect_s3_class(as_geo(quakes,colname = 'geom_wkt')
                  ,class = 'sf')

  # passing a data.frame with a valid WKT character column number
  expect_s3_class(as_geo(quakes,colname = 6)
                  ,class = 'sf')

  # when passing a sf object, save the old geom column into orig geom and create a new one based on provided colname.
  expect_s3_class(
    as_geo(quakes_sf,colname = 4)
    ,class = 'sf'
  )
  expect_s3_class(
    as_geo(quakes_sf,colname = 4)
    ,class = 'sf'
  )

  # the orig_geom column is created
  expect_true('orig_geom' %in% colnames(as_geo(quakes_sf,colname = 'geom_wkt')))
  expect_true('orig_geom' %in% colnames(as_geo(quakes_sf,colname = 4)))

  # errors

  expect_error(as_geo(quakes,colname = 'geomatry'))

  expect_error(as_geo(quakes,colname = 10))

  expect_error(as_geo(quakes,colname = 'geom_bla'))

  expect_error(as_geo(quakes,colname = 7))

  #if provided geom column is the current geom column, do nothing
  # expect_output(as_geo(quakes_sf))

  expect_output(as_geo(quakes$geom_bla)
                 ,class = 'sf')

})






