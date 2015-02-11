#' Feature: Statistics of Nearest-Neighbour distances.
#'
#' @template arg_network
#' @return [\code{list}]
#' @export
getNearestNeighbourFeatureSet = function(x) {
    assertClass(x, "Network")
    #FIXME: dists need to be saved in netgen
    measureTime(expression({
        nn.dists = getNearestNeighbourDistancesCPP(as.matrix(x$distance.matrix))
        computeStatisticsOnNumericVector(nn.dists, "nearest_neighbour")
    }))
}
