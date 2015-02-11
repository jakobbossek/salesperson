#' Feature: number of points on convex hull and spanning area.
#'
#' @template arg_network
#' @return [\code{list}]
#' @export
getConvexHullFeatureSet = function(x) {
    assertClass(x, "Network")
    # here we delegate to tspmeta
    measureTime(expression({
        tsp.instance = tspmeta::tsp_instance(x$coordinates, dists = x$distance.matrix)
        tspmeta::feature_chull(tsp.instance)
    }))
}
