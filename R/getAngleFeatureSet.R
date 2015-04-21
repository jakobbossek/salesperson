#' Feature: statistics of angles between nodes and their two nearest neighbors.
#'
#' @template arg_network
#' @template arg_include_costs
#' @return [\code{list}]
#' @export
getAngleFeatureSet = function(x, include.costs = FALSE) {
  assertClass(x, "Network")
  measureTime(expression({
    angles = getAnglesToNearestNeighborsCPP(x$coordinates, as.matrix(x$distance.matrix))
    computeStatisticsOnNumericVector(angles, "angle")
  }), "angle", include.costs)
}
