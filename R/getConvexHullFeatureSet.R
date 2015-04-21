#' Feature: number of points on convex hull and spanning area.
#'
#' @template arg_network
#' @template arg_include_costs
#' @return [\code{list}]
#' @export
getConvexHullFeatureSet = function(x, include.costs = FALSE) {
  assertClass(x, "Network")
  # here we delegate to tspmeta
  tsp.instance = netgenToTSPmeta(x)
  measureTime(expression({
    tspmeta::feature_chull(tsp.instance)
  }), "chull", include.costs)
}
