#' Feature: Centroid coordinates and statistics of distances to centroid.
#'
#' @template arg_network
#' @template arg_include_costs
#' @param normalize [\code{logical(1)}]\cr
#'   Additionally calculate the normalization for the features? The default is
#'   \code{FALSE}.
#' @template arg_dots
#' @return [\code{list}]
#' @export
getCentroidFeatureSet = function(x, include.costs = FALSE, normalize = FALSE, ...) {
  assertClass(x, "Network")
  measureTime(expression({
    centroid.coordinates = getCentroidCoordinatesCPP(x$coordinates)
    distances.to.centroid = getDistancesToCentroidCPP(x$coordinates, centroid.coordinates)
    coord = x$coordinates
    width = getWidth(coord)
    height = getHeight(coord)
    statistics.on.distances.to.centroid = computeStatisticsOnNumericVector(distances.to.centroid, "centroid", normalize = normalize)
    n_cities = getNumberOfNodes(x)
    if (!normalize) {
      return(c(list(
        "centroid_x" = centroid.coordinates[1],
        "centroid_y" = centroid.coordinates[2]
      ), statistics.on.distances.to.centroid))
    }
      list(
      "centroid_x" = normalizeFeature(centroid.coordinates[1], max(coord[, 1]) * (n_cities - 1) / n_cities + min(coord[, 1]) / n_cities, min(coord[, 1]) * (n_cities - 1) / n_cities + max(coord[, 1]) / n_cities),
      "centroid_y" = normalizeFeature(centroid.coordinates[2], max(coord[, 2]) * (n_cities - 1) / n_cities + min(coord[, 2]) / n_cities, min(coord[, 2]) * (n_cities - 1) / n_cities + max(coord[, 2]) / n_cities),
      "centroid_mean" = normalizeFeature(statistics.on.distances.to.centroid[["centroid_mean"]], norm(c(width, height), type = "2") / 2, norm(c(width, height), type = "2") / n_cities),
      "centroid_sd" = NA,
      "centroid_var" = statistics.on.distances.to.centroid[["centroid_norm_var"]],
      "centroid_median" = normalizeFeature(statistics.on.distances.to.centroid[["centroid_median"]], computeMaxMedian(n_cities, width, height)),
      "centroid_varcoeff" = NA,
      "centroid_min" = normalizeFeature(statistics.on.distances.to.centroid[["centroid_min"]], norm(c(width, height), type = "2") / 2),
      "centroid_max" = normalizeFeature(statistics.on.distances.to.centroid[["centroid_max"]], norm(c(width, height), type = "2") * ((n_cities - 1) / n_cities), max(width, height) / 2),
      "centroid_span" = normalizeFeature(statistics.on.distances.to.centroid[["centroid_span"]], norm(c(width, height), type = "2") * ((n_cities - 2) / n_cities)),
      "centroid_skew" = NA
      )
  }), "centroid", include.costs)
}

computeMaxMedian = function(n, width, height) {
  if (n %% 2 == 0) {
    r = if ((n / 2) %% 2 == 0) n / 2 - 2 else n / 2 - 1
  } else {
    r = if (floor(n / 2) %% 2 == 0) floor(n / 2) - 1 else floor(n / 2)
  }
  cx = (width * (n - r) / 2) / n
  cy = (height * (n - r) / 2) / n
  return(max(min(norm(c(cx, height - cy), type="2"), norm(c(cy, width - cx), type = "2")), norm(c(width, height), type="2")))
}
