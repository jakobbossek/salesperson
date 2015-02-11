#' Feature: statistics of angles between nodes and their two nearest neighbors.
#'
#' @template arg_network
#' @return [\code{list}]
#' @export
getAngleFeatureSet = function(x) {
    assertClass(x, "Network")
    measureTime(expression({
        angles = getAnglesToNearestNeighborsCPP(x$coordinates, as.matrix(x$distance.matrix))
        computeStatisticsOnNumericVector(angles, "angle")
    }))
}