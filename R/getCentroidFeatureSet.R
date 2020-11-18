#' Feature: Centroid coordinates and statistics of distances to centroid.
#'
#' @template arg_network
#' @template arg_include_costs
#' @template arg_dots
#' @return [\code{list}]
#' @export
getCentroidFeatureSet = function(x, include.costs = FALSE, ...) {
  assertClass(x, "Network")
  measureTime(expression({
    centroid.coordinates = getCentroidCoordinatesCPP(x$coordinates)
    distances.to.centroid = getDistancesToCentroidCPP(x$coordinates, centroid.coordinates)
    width = getWidth(x$coordinates)
    height = getHeight(x$coordinates)
    statistics.on.distances.to.centroid = computeStatisticsOnNumericVector(distances.to.centroid, "centroid")
    n_cities = getNumberOfNodes(x)

    c(list(
      "centroid_x" = centroid.coordinates[1],
      "centroid_y" = centroid.coordinates[2]
      ), statistics.on.distances.to.centroid, 
      list(
      "centroid_norm_x" = centroid.coordinates[1] / width,
      "centroid_norm_y" = centroid.coordinates[2] / height,
      "centroid_norm_mean" = normalizeFeature(statistics.on.distances.to.centroid[["centroid_mean"]], norm(c(width, height), type="2") / 2),
      "centroid_norm_median" = normalizeFeature(statistics.on.distances.to.centroid[["centroid_median"]], computeMaxMedian(n_cities, width, height)),
      "centroid_norm_min" = normalizeFeature(statistics.on.distances.to.centroid[["centroid_min"]], norm(c(width, height), type="2") / 2),
      "centroid_norm_max" = normalizeFeature(statistics.on.distances.to.centroid[["centroid_max"]], norm(c(width, height), type="2") * ((n_cities-1) / n_cities)),
      "centroid_norm_span" = normalizeFeature(statistics.on.distances.to.centroid[["centroid_span"]], norm(c(width, height), type="2") * ((n_cities-2) / n_cities))
      ))
  }), "centroid", include.costs)
}

computeMaxMedian = function(n, width, height){
  if (n %% 2 == 0){
    r = if ((n / 2) %% 2 == 0) n / 2 - 2 else n / 2 - 1
  }else{
    r = if (floor(n / 2) %% 2 == 0) floor(n / 2) - 1 else floor(n / 2)
  }
  cx = (width * (n - r) / 2) / n
  cy = (height * (n - r) / 2) / n
  return(max(min(norm(c(cx, height-cy), type="2"), norm(c(cy, width-cx), type = "2")), norm(c(width, height), type="2")))
}
