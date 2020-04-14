#' Feature: Statistics of Nearest-Neighbour distances.
#'
#' @template arg_network
#' @template arg_include_costs
#' @template arg_dots
#' @return [\code{list}]
#' @export
getNearestNeighbourFeatureSet = function(x, include.costs = FALSE, ...) {
  assertClass(x, "Network")
  measureTime(expression({
    nn.dists = getNearestNeighbourDistancesCPP(x$distance.matrix)
    computeStatisticsOnNumericVector(nn.dists, "nearest_neighbour")
  }), "nearest_neighbor", include.costs)
}
