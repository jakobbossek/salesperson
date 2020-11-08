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
    statistics.on.nn.dists = computeStatisticsOnNumericVector(nn.dists, "nearest_neighbour")
    d.max = getDMax(x$coordinates)
    dist.max = computeL2Norm(c(getWidth(x$coordinates), getHeight(x$coordinates)))
    c(statistics.on.nn.dists,
      "nearest_neighbour_norm_mean" = normalizeFeature(statistics.on.nn.dists$nearest_neighbour_mean, d.max),
      "nearest_neighbour_norm_median" = normalizeFeature(statistics.on.nn.dists$nearest_neighbour_median, d.max),
      "nearest_neighbour_norm_min" = normalizeFeature(statistics.on.nn.dists$nearest_neighbour_min, d.max),
      "nearest_neighbour_norm_max" = normalizeFeature(statistics.on.nn.dists$nearest_neighbour_max, dist.max),
      "nearest_neighbour_norm_span" = normalizeFeature(statistics.on.nn.dists$nearest_neighbour_span, dist.max)
      )
  }), "nearest_neighbor", include.costs)
}
