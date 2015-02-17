#' Feature: Statistics of Nearest-Neighbour distances.
#'
#' @template arg_network
#' @template arg_include_costs
#' @return [\code{list}]
#' @export
getNearestNeighbourFeatureSet = function(x, include.costs = FALSE) {
    assertClass(x, "Network")
    #FIXME: dists need to be saved in netgen
    measureTime(expression({
        nn.dists = getNearestNeighbourDistancesCPP(as.matrix(x$distance.matrix))
        computeStatisticsOnNumericVector(nn.dists, "nearest_neighbour")
    }), "nearest_neighbor", include.costs)
}
