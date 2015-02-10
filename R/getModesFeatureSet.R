#' Feature: modes features.
#'
#' @template arg_network
#' @return [\code{list}]
#' @export
getModesFeatureSet = function(x) {
    # here we delegate to tspmeta
    tsp.instance = tspmeta::tsp_instance(x$coordinates)
    tspmeta::feature_modes(tsp.instance)
}
