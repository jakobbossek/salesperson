#' Feature: Statistics of Nearest-Neighbour distances.
#'
#' @template arg_network
#' @template arg_include_costs
#' @template arg_normalize
#' @template arg_dots
#' @return [\code{list}]
#' @export
getNearestNeighbourFeatureSet = function(x, include.costs = FALSE, normalize = FALSE, ...) {
  assertClass(x, "Network")
  measureTime(expression({
    nn.dists = getNearestNeighbourDistancesCPP(x$distance.matrix)
    statistics.on.nn.dists = computeStatisticsOnNumericVector(nn.dists, "nearest_neighbour", normalize = normalize)
    if (!normalize) {
      return(statistics.on.nn.dists)
    }
    n.cities = nrow(x$coordinates)
    a = getWidth(x$coordinates)
    b = getHeight(x$coordinates)
    d.max = getDMax(x$coordinates)
    dist.max = computeL2Norm(c(a, b))
    list(
      "nearest_neighbour_mean" = normalizeFeature(statistics.on.nn.dists$nearest_neighbour_mean, d.max, dist.max / n.cities),
      "nearest_neighbour_sd" = NA,
      "nearest_neighbour_var" = statistics.on.nn.dists$nearest_neighbour_norm_var,
      "nearest_neighbour_median" = normalizeFeature(statistics.on.nn.dists$nearest_neighbour_median, getDMax(x$coordinates, ceiling(n.cities / 2) + 2)),
      "nearest_neighbour_varcoeff" = NA,
      "nearest_neighbour_min" = normalizeFeature(statistics.on.nn.dists$nearest_neighbour_min, d.max),
      "nearest_neighbour_max" = normalizeFeature(statistics.on.nn.dists$nearest_neighbour_max, dist.max, dist.max / n.cities),
      "nearest_neighbour_span" = normalizeFeature(statistics.on.nn.dists$nearest_neighbour_span, dist.max),
      "nearest_neighbour_skew" = NA
      )
  }), "nearest_neighbor", include.costs)
}
