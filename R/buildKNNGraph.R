# Constructs the k-Nearest-Neighbor (k-NN) graph.
#
# @param x [\code{Network}]\cr
#   Source network.
# @param j [\code{integer(1)}]\cr
#   Desired number of neighbors.
# @return [\code{}]
buildKNNGraph = function(x, k = 1L) {
  d = x$distance.matrix
  knn = list()
  n = ncol(d)
  for (i in seq(n)) {
    knn[[i]] = order(d[i, ])[2:(k + 1L)]
  }
  return(knn)
}
