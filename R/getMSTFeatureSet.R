#' Feature: statistics of the minimum spanning tree edges.
#'
#' @template arg_network
#' @template arg_include_costs
#' @return [\code{list}]
#' @export
getMSTFeatureSet = function(x, include.costs = FALSE) {
    assertClass(x, "Network")
    # here we delegate to tspmeta
    tsp.instance = tspmeta::tsp_instance(x$coordinates, dists = x$distance.matrix)
    measureTime(expression({
        tspmeta::feature_mst(tsp.instance)
    }), "mst", include.costs)
}
