#' Feature: Statistics of Nearest-Neighbour distances.
#'
#' @template arg_network
#' @template arg_include_costs
#' @template arg_dots
#' @return [\code{list}]
#' @export
getDistanceFeatureSet = function(x, include.costs = FALSE, ...) {
  assertClass(x, "Network")
  measureTime(expression({
    dist.obj = x$distance.matrix
    dist.mat = as.matrix(dist.obj)
    dist.num = as.numeric(as.dist(dist.obj))
    n.cities = nrow(dist.mat)
    feat.set = getDistanceFeatureSetCPP(dist.mat, dist.num)
    feat.set = c(feat.set, computeStatisticsOnNumericVector(dist.num, "distance"))
    return(feat.set)
  }), "distance", include.costs)
}
