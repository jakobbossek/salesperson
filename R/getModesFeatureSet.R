#' Feature: modes features.
#'
#' @template arg_network
#' @template arg_include_costs
#' @return [\code{list}]
#' @export
getModesFeatureSet = function(x, include.costs = FALSE) {
    assertClass(x, "Network")
    # here we delegate to tspmeta
    tsp.instance = tspmeta::tsp_instance(x$coordinates, dists = x$distance.matrix)
    measureTime(expression({
        tspmeta::feature_modes(tsp.instance)
    }), "modes", include.costs)
}
