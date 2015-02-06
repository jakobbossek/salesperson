#' Feature: Centroid coordinates and statistics of distances to centroid.
#'
#' @template arg_network
#' @return [\code{list}]
#' @export
getCentroidFeatureSet = function(x) {
    assertClass(x, "Network")
    centroid.coordinates = getCentroidCoordinatesCPP(x$coordinates)
    distances.to.centroid = getDistancesToCentroidCPP(x$coordinates, centroid.coordinates)
    return(c(list(
        "centroid_x" = centroid.coordinates[1],
        "centroid_y" = centroid.coordinates[2]
    ), computeStatisticsOnNumericVector(distances.to.centroid, "centroid")))
}
