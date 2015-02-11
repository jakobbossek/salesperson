#' Feature: modes features.
#'
#' @template arg_network
#' @return [\code{list}]
#' @export
getModesFeatureSet = function(x) {
    assertClass(x, "Network")
    # here we delegate to tspmeta
    measureTime(expression({
        tsp.instance = tspmeta::tsp_instance(x$coordinates, dists = x$distance.matrix)
        tspmeta::feature_modes(tsp.instance)
    }))
}
