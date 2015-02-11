#' Feature: modes features.
#'
#' @template arg_network
#' @return [\code{list}]
#' @export
getModesFeatureSet = function(x) {
    assertClass(x, "Network")
    # here we delegate to tspmeta
    tsp.instance = tspmeta::tsp_instance(x$coordinates, dists = x$distance.matrix)
    measureTime(expression({
        tspmeta::feature_modes(tsp.instance)
    }))
}
