#' Feature: modes features.
#'
#' @template arg_network
#' @template arg_include_costs
#' @return [\code{list}]
#' @export
getModesFeatureSet = function(x, include.costs = FALSE) {
    assertClass(x, "Network")
    # here we delegate to tspmeta
    tsp.instance = netgenToTSPmeta(x)
    measureTime(expression({
        tspmeta::feature_modes(tsp.instance)
    }), "modes", include.costs)
}
