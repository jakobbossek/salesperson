#' Feature: Statistics of Nearest-Neighbour distances.
#'
#' @template arg_network
#' @template arg_include_costs
#' @template arg_normalize
#' @template arg_dots
#' @return [\code{list}]
#' @export
getDistanceFeatureSet = function(x, include.costs = FALSE, normalize = FALSE, ...) {
  assertClass(x, "Network")
  measureTime(expression({
    dist.obj = x$distance.matrix
    dist.mat = as.matrix(dist.obj)
    dist.num = as.numeric(as.dist(dist.obj))
    feat.set = getDistanceFeatureSetCPP(dist.mat, dist.num)
    statistics.on.distances = computeStatisticsOnNumericVector(dist.num, "distance", normalize = normalize)
    if (!normalize) {
      feat.set = c(feat.set, statistics.on.distances)
    } else {
      n.cities = nrow(dist.mat)
      width = getWidth(x$coordinates)
      height = getHeight(x$coordinates)
      d.max = getDMax(x$coordinates)
      feat.set = c("fraction_shorter_mean_distance" = normalizeFeature(feat.set$fraction_shorter_mean_distance, 1 - 2 / n.cities, n.cities * (n.cities / 4 - 1) / (n.cities * (n.cities - 1))),
                   "fraction_of_distinct_distances" = normalizeFeature(feat.set$fraction_of_distinct_distances, 1, 2 / (n.cities * (n.cities - 1) / 2)),
                   "mode_frequency" = normalizeFeature(feat.set$mode_frequency, (n.cities - 2) * (n.cities - 1) / 2, 1),
                   "mode_quantity" = normalizeFeature(feat.set$mode_quantity, n.cities * (n.cities - 1) / 2, 1),
                   "mode_mean" = normalizeFeature(feat.set$mode_mean, computeL2Norm(c(width, height))),
                   "mean_tour_length" = normalizeFeature(feat.set$mean_tour_length, (n.cities ** 2 * (width + height + computeL2Norm(c(width, height)))) / (4 * (n.cities - 1)), 2 * computeL2Norm(c(width, height))),
                   "sum_of_lowest_edge_values" = normalizeFeature(feat.set$sum_of_lowest_edge_values, n.cities * d.max),
                   "distance_mean" = normalizeFeature(statistics.on.distances$distance_mean, (n.cities * (width + height + computeL2Norm(c(width, height)))) / (4 * (n.cities - 1)), (2 * computeL2Norm(c(width, height))) / n.cities),
                   "distance_sd" = NA,
                   "distance_var" = statistics.on.distances$distance_norm_var,
                   "distance_median" = normalizeFeature(statistics.on.distances$distance_median, computeL2Norm(c(width, height))),
                   "distance_varcoeff" = NA,
                   "distance_min" = normalizeFeature(statistics.on.distances$distance_min, d.max),
                   "distance_max" = normalizeFeature(statistics.on.distances$distance_max, computeL2Norm(c(width, height)), max(width, height)),
                   "distance_span" = normalizeFeature(statistics.on.distances$distance_span, computeL2Norm(c(width, height)), max(width, height) - d.max),
                   "distance_skew" = NA

      )
    }
    return(feat.set)
  }), "distance", include.costs)
}




# Get the maximal possible minimal distance of one city to another city
#
# @param coord [matrix]
#   The coordinate matrix of the instance.
# @param n [numeric]
#   The number of nodes that should be used for the calculation 
#   if deviating from the number of nodes in the coordinate matrix. 
#   If it is <= 1 (default) assume no deviation.
# @return [numeric]
getDMax = function(coord, n = 0) {
  a = getWidth(coord)
  b = getHeight(coord)
  if(n <= 1){
    n = nrow(coord)
  }
  p = (a + b) / 2
  r.max = (p + sqrt(p ** 2 + 4 * (n - 1) * a * b / (2 * sqrt(3)))) / (2 * (n - 1))
  return(2 * r.max)
}

