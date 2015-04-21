# Convert netgen object to tspmeta TSP instance.
#
# @param x [Network]
#   Network.
# @return [tsp_instance]
netgenToTSPmeta = function(x) {
  structure(list(coords = x$coordinates, dists = x$distance.matrix),
    class = c("tsp_instance_euclidean_coords", "tsp_instance_symmetric", "tsp_instance"))
}

# Check if vector contains permutation of number 1:n
#
# @param x [\code{numeric}]\cr
#   Vector to check.
# @param source [\code{numeric}]\cr
#   Vector which we want to compare with. Default is 1, ..., length(x).
# @return [\code{logical(1)}]
isPermutation = function(x, source = seq(length(x))) {
  return(all(source == sort(x)))
}
