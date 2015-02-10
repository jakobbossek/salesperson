#' Feature: number of points on convex hull and spanning area.
#'
#' @template arg_network
#' @return [\code{list}]
#' @export
getConvexHullFeatureSet = function(x) {
    # here we delegate to tspmeta
    tsp.instance = tspmeta::tsp_instance(x$coordinates)
    tspmeta::feature_chull(tsp.instance)
}
