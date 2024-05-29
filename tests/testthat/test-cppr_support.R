
test_that('make_lines',{
  data("lille_graph")

  rand_od <- lille_graph$coords$osmid |> sample(2)

  path <- cppRouting::get_path_pair(Graph = lille_graph
                                    ,from = rand_od[1]
                                    ,to = rand_od[2])

  # success

  expect_s3_class(
    make_lines(paths=path
               ,graph=lille_graph)
    ,class = 'sf'
  )

  expect_true(nrow(make_lines(paths=path
                              ,graph=lille_graph))==1)

  expect_true(sf::st_geometry_type(make_lines(paths=path
                                              ,graph=lille_graph))=='LINESTRING')

  # error

})


test_that('fnearest_node',{

  data("lille_graph")
  data("lille_amenities")

  loc_crs <- 27561

  sample_amen <- lille_amenities |>
    samp_dt(10) |>
    sf::st_coordinates()

  sample_amen_proj <- lille_amenities |>
    samp_dt(10) |>
    sf::st_transform(27561) |>
    sf::st_coordinates()


  lille_graph_proj <- lille_graph

  lille_graph_proj$coords[,c(2,3)] <- lille_graph$coords |> sf::st_as_sf(coords=c(2,3),crs=4326) |>
    sf::st_transform(loc_crs) |>
    sf::st_coordinates() |>
    as.data.frame()

  # success
  #the standard workflow
  expect_type(object=fnearest_nodes(graph = lille_graph
                                    ,pts = sample_amen
                                    ,local_crs = loc_crs)
              ,type='list')


  # parameters are in proj crs
  expect_type(object=fnearest_nodes(graph = lille_graph_proj
                                    ,pts = sample_amen_proj
                                    # ,local_crs = loc_crs
                                    )
              ,type='list')

  # parameters are in crs=4326, and loc_crs is provided
  expect_type(
    fnearest_nodes(graph = lille_graph
                   ,pts = lille_amenities |> samp_dt(10)
                   ,local_crs = loc_crs)
    ,type='list'
  )

  # errors

  # if local crs is provided but data is not crs=4326, notify
  expect_message(fnearest_nodes(graph = lille_graph
                                ,pts = lille_amenities |> samp_dt(10) |> sf::st_transform(loc_crs)
                                ,local_crs = loc_crs)
                 )

  # no local crs is provided, but graph nodes are likely to be crs 4326
  expect_warning(fnearest_nodes(graph = lille_graph
                                ,pts = sample_amen_proj
                                # ,local_crs = loc_crs
                                ))

  expect_error(
    fnearest_nodes(graph = lille_graph_proj
                   ,pts = lille_amenities |> samp_dt(10)
                   ,local_crs = loc_crs)
  )


  # no local crs is provided, but graph nodes are likely to be crs 4326
  expect_warning(fnearest_nodes(graph = lille_graph
                                ,pts = sample_amen)
  )

  expect_error(
    fnearest_nodes(graph = lille_graph_proj
                   ,pts = lille_amenities |> samp_dt(10))
  )

  expect_error(
    fnearest_nodes(graph = lille_graph
                   ,pts = lille_amenities |> samp_dt(10))
  ) |>
    suppressWarnings()

  # bad pts data with or without loc_crs
  expect_error(
    fnearest_nodes(graph = lille_graph
                   ,pts = cbind(sample_amen,rep_len(0,nrow(sample_amen))))
  ) |>
    suppressWarnings()

  expect_error(
    fnearest_nodes(graph = lille_graph
                   ,pts = cbind(sample_amen,rep_len(0,nrow(sample_amen)))
                   ,local_crs = loc_crs)
  )

  # bad nn value
  expect_error(
    fnearest_nodes(graph = lille_graph
                   ,pts = sample_amen
                   ,local_crs = loc_crs
                   ,nn='abra')
  )

  expect_error(
    fnearest_nodes(graph = lille_graph
                   ,pts = sample_amen
                   ,local_crs = loc_crs
                   ,nn=-1)
  )

  #bad crs value
  expect_error(
    fnearest_nodes(graph = lille_graph
                   ,pts = sample_amen
                   ,local_crs = 'abra'
                   )
  )

  expect_error(
    fnearest_nodes(graph = lille_graph
                   ,pts = sample_amen
                   ,local_crs = loc_crs
                   ,crs=-1)
  )
})


test_that('get_lcc',{
  data("lille_graph")

  data("nodes")
  data("segments")
  # success

  #this graph is already fully connected, so the user is notified
  expect_output(
    get_lcc(ways = lille_graph$data)
  )

  #this one is not connected, so the output should be the lcc
  expect_s3_class(
    object = get_lcc(ways = segments)
    ,class = 'data.frame'
  )

  expect_s3_class(
    get_lcc(ways = lille_graph$data)
    ,class = 'data.frame'
  )

  # return is non-empty
  expect_true(nrow(get_lcc(ways = lille_graph$data))>0)

  # return is non-empty
  expect_true(nrow(get_lcc(ways=segments))>0)


  # errors

  expect_error(
    get_lcc(lille_graph)
  )

  expect_error(
    get_lcc(lille_graph$data[-1])
  )

})


test_that('make_network',{

  data("lille_graph")

  data("nodes")
  data("segments")

  # ways <- lille_graph$data
  # nodes <- lille_graph$coords

  # success
  expect_type(
    make_network(edges = segments
                 ,simple = FALSE)
    ,type = 'list'
  )

  # errors
  expect_error(
    make_network(edges = ways
                 ,nodes = nodes
                 ,simple = FALSE)
  )
})

