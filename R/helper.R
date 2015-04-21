# Convert netgen object to tspmeta TSP instance.
#
# @param x [Network]
#   Network.
# @return [tsp_instance]
netgenToTSPmeta = function(x) {
  structure(list(coords = x$coordinates, dists = x$distance.matrix),
    class = c("tsp_instance_euclidean_coords", "tsp_instance_symmetric", "tsp_instance"))
}
