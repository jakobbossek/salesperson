#' Feature: statistics of the minimum spanning tree edges.
#'
#' @template arg_network
#' @return [\code{list}]
#' @export
getMSTFeatureSet = function(x) {
    # here we delegate to tspmeta
    tsp.instance = tspmeta::tsp_instance(x$coordinates)
    tspmeta::feature_mst(tsp.instance)
}
