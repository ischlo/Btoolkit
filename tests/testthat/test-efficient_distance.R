test_that("efficient_distance", {

  data("london_msoa")

  # success
  # single value WKT
  expect_type(
    fdistance(coord1=london_msoa$centroid[1]
              ,coord2=london_msoa$centroid[2]
              ,crs=4326
              # ,one_to_one = FALSE
    )
    ,type = "double"
  )


  expect_true(
    length(fdistance(coord1=london_msoa$centroid[1]
                  ,coord2=london_msoa$centroid[2]
                  ,crs=4326
                  ))==1
  )



})
