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
    width = getWidth(x$coordinates)
    height = getHeight(x$coordinates)
    d.max = getDMax(x$coordinates)
    feat.set = getDistanceFeatureSetCPP(dist.mat, dist.num)
    statistics.on.distances = computeStatisticsOnNumericVector(dist.num, "distance")
    feat.set = c(feat.set, statistics.on.distances,
                 "fraction_shorter_mean_distance_norm" = normalizeFeature(feat.set$fraction_shorter_mean_distance, 1 - 2 / n.cities, n.cities * (n.cities / 4 - 1) / (n.cities * (n.cities - 1))),
                 "fraction_of_distinct_distances_norm" = normalizeFeature(feat.set$fraction_of_distinct_distances, 1, 1 / (n.cities * (n.cities - 1) / 2)),
                 "mode_frequency_norm" = normalizeFeature(feat.set$mode_frequency, n.cities * (n.cities - 1) / 2, 1),
                 "mode_quantity_norm" = normalizeFeature(feat.set$mode_quantity, n.cities * (n.cities - 1) / 2, 1),
                 "mode_mean_norm" = normalizeFeature(feat.set$mode_mean, computeL2Norm(c(width, height))),
                 "mean_tour_length_norm" = normalizeFeature(feat.set$mean_tour_length, (n.cities ** 2 * (width + height + computeL2Norm(c(width, height)))) / (4 * (n.cities - 1))),
                 "sum_of_lowest_edge_values_norm" = normalizeFeature(feat.set$sum_of_lowest_edge_values, n.cities * d.max),
                 "distance_mean_norm" = normalizeFeature(statistics.on.distances$distance_mean, (n.cities * (width + height + computeL2Norm(c(width, height)))) / (4 * (n.cities - 1))),
                 "distance_median_norm" = normalizeFeature(statistics.on.distances$distance_median, computeL2Norm(c(width, height))),
                 "distance_min_norm" = normalizeFeature(statistics.on.distances$distance_min, d.max),
                 "distance_max_norm" = normalizeFeature(statistics.on.distances$distance_max, computeL2Norm(c(width, height))),
                 "distance_span_norm" = normalizeFeature(statistics.on.distances$distance_span, computeL2Norm(c(width, height)))
                 
    )
    return(feat.set)
  }), "distance", include.costs)
}




# Get the maximal possible minimal distance of one city two another city
#
# @param coord [matrix]
#   The coordinates matrix from the instance.
# @return [numeric]
getDMax = function(coord){
  a = getWidth(coord)
  b = getHeight(coord)
  p = a / 2 + b / (2 * sqrt(3))
  n = nrow(coord)
  r.max = (p + sqrt(p ** 2 + 4 * n * a * b / (2 * sqrt(3)))) / (2 * n)
  return(2 * r.max)
}
