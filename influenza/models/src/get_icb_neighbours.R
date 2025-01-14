# creates neighbours list object of ICBs consisting of:
#
# adjacency network (shapefiles sharing at least one coordinate)
# and minimal spanning tree based on icb_name distance matrix
#
# and saves locally

mstconnect <- function(polys, nb, distance = "centroid") {
  # get union of:
  # minimal spanning tree of polys spatial data
  # nb neighbours object

  # generate distance matrix
  if (distance == "centroid") {
    coords <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(polys)))
    dmat <- as.matrix(dist(coords))
  } else if (distance == "polygon") {
    dmat <- sf::st_distance(polys) + 1 # offset for adjacencies
    diag(dmat) <- 0 # no self-intersections
  } else {
    stop("Unknown distance method")
  }

  # get minimal spanning tree (mst)
  graph_full <- igraph::graph.adjacency(dmat, weighted = TRUE, mode = "undirected")
  graph_mst <- igraph::mst(graph_full)
  # mst to matrix of adjacent edges
  edge_mat <- as.matrix(igraph::as_adj(graph_mst))
  edge_weights_list <- spdep::mat2listw(edge_mat)
  edge_nb <- edge_weights_list$neighbour
  attr(edge_nb, "region.id") <- attr(nb, "region.id")
  # get union of mst and nb
  allnb <- spdep::union.nb(nb, edge_nb)
  allnb

}

# get spatial icb_name data
spatial_ICB <- aws.s3::s3read_using(
  object = "PATH REMOVED/Integrated_Care_Boards_April_2023_EN_BFC_1659257819249669363.zip",
  FUN = unzip,
  exdir = tempdir()
) %>%
  stringr::str_subset(".shp$") %>%
  sf::st_read() %>%
  dplyr::rename(icb_name = ICB23NM, icb_code = ICB23CD) %>%
  dplyr::arrange(icb_name) %>%
  dplyr::mutate(icb_name = as.factor(icb_name))

rownames(spatial_ICB) <- spatial_ICB$icb_name

reference_ids <- data.frame(
  icb_name = spatial_ICB$icb_name,
  region.id = 1:length(spatial_ICB$icb_name)
)

# construct neighbours list
nbobj <- spdep::poly2nb(spatial_ICB, queen = TRUE, row.names = unique(train_data$icb_name))
names(nbobj) <- attr(nbobj, "region.id")

# union with minimal spanning tree
nbobj_mst <- mstconnect(spatial_ICB, nbobj)
levels <- factor(attr(nbobj_mst, "region.id"), levels = attr(nbobj_mst, "region.id"))
nbobj_mst <- nbobj_mst %>%
  lapply(function(xx) levels[xx] %>% factor(levels = levels)) %>%
  set_names(levels)

# save files locally
saveRDS(nbobj, file = "./influenza/icb_neighbours.RData")
saveRDS(nbobj_mst, file = "./influenza/icb_neighbours_with_minimal_spanning_tree.RData")
