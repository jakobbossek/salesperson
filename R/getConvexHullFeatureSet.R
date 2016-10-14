#' Feature: number of points on convex hull and spanning area.
#'
#' @template arg_network
#' @template arg_include_costs
#' @return [\code{list}]
#' @export
getConvexHullFeatureSet = function(x, include.costs = FALSE) {
  assertClass(x, "Network")
  measureTime(expression({
    getConvexHullFeatureSet2(x)
  }), "chull", include.costs)
}

getConvexHullFeatureSet2 = function(x) {
  coordinates = x$coordinates
  hull = chull(coordinates[, 1L], coordinates[, 2L])
  area = splancs::areapl(coordinates[hull, ])
  list(
    chull_area = area,
    chull_points_on_hull = length(hull) / getNumberOfNodes(x)
  )
}
