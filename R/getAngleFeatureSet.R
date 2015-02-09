#' Feature: statistics of angles between nodes and their two nearest neighbors.
#'
#' @template arg_network
#' @return [\code{list}]
#' @export
getAngleFeatureSet = function(x) {
    assertClass(x, "Network")
    angles = getAnglesToNearestNeighborsCPP(x$coordinates, as.matrix(x$distance.matrix))
    return(computeStatisticsOnNumericVector(angles, "angle"))
}