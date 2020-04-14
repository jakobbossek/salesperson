#' Feature: statistics of the minimum spanning tree edges.
#'
#' @template arg_network
#' @template arg_include_costs
#' @template arg_dots
#' @return [\code{list}]
#' @export
getMSTFeatureSet = function(x, include.costs = FALSE, ...) {
  assertClass(x, "Network")
  measureTime(expression({
    getMSTFeatureSet2(x)
  }), "mst", include.costs)
}

getMSTFeatureSet2 = function(x) {
  d = x$distance.matrix
  # compute spanning tree
  span_tree = spantree(d)
  # depths of MST
  span_tree_depth = spandepth(span_tree)
  # distances within MST
  span_tree_dists = span_tree$dist

  res = c(
    computeStatisticsOnNumericVector(span_tree_depth, "mst_depth"),
    computeStatisticsOnNumericVector(span_tree_dists, "mst_dists")
  )

  res$mst_dists_sum = sum(span_tree_dists) / sum(d)
  return(res)
}
