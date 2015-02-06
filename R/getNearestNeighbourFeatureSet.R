#' Feature: Statistics of Nearest-Neighbour distances.
#'
#' @template arg_network
#' @return [\code{list}]
#' @export
getNearestNeighbourFeatureSet = function(x) {
    assertClass(x, "Network")
    #FIXME: dists need to be saved in netgen
    nn.dists = getNearestNeighbourDistancesCPP(as.matrix(dist(x$coordinates)))
    return(computeStatisticsOnNumericVector(nn.dists, "nearest_neighbour"))
}
