#' Feature: statistics of the minimum spanning tree edges.
#'
#' @template arg_network
#' @param epsilon [\code{numeric(1)}]\cr
#'   Probability for reachability computation in dbscan clustering method.
#'   See \code{\link[tspmeta]{feature_cluster}}. Default is 0.01.
#' @template arg_include_costs
#' @return [\code{list}]
#' @export
getClusterFeatureSet = function(x, epsilon = 0.01, include.costs = FALSE) {
    assertNumber(epsilon, lower = 0.001, upper = 1, na.ok = FALSE)
    # here we delegate to tspmeta
    tsp.instance = netgenToTSPmeta(x)
    measureTime(expression({
        tspmeta::feature_cluster(tsp.instance, epsilon = epsilon)
    }), paste("cluster", epsilon, sep = "_"), include.costs)
}
