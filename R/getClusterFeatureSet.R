#' Feature: statistics of the minimum spanning tree edges.
#'
#' @template arg_network
#' @param epsilon [\code{numeric(1)}]\cr
#'   Probability for reachability computation in dbscan clustering method.
#'   Default is 0.01.
#' @template arg_include_costs
#' @template arg_dots
#' @return [\code{list}]
#' @export
getClusterFeatureSet = function(x, epsilon = 0.01, include.costs = FALSE, ...) {
  assertNumber(epsilon, lower = 0.001, upper = 1, na.ok = FALSE)
  # here we delegate to tspmeta
  measureTime(expression({
    getClusterFeatureSet2(x, epsilon = epsilon)
  }), paste("cluster", epsilon, sep = "_"), include.costs)
}


getClusterFeatureSet2 = function(x, epsilon) {
  coordinates = x$coordinates
  d = as.vector(x$distance.matrix)
  # FIXME: Really strip 0 distances?
  d = d[d > 0]
  q = quantile(d, epsilon)
  # do the clustering
  fit = fpc::dbscan(coordinates, q, showplot = FALSE)

  # skip singleton clusters.
  real.clusters = which(fit$cluster > 0)

  cm = fit$cluster[real.clusters]
  coordinates = coordinates[real.clusters, , drop = FALSE]
  if (length(cm) > 0) {
    distances = sapply(unique(cm), function(cluster) {
      cluster.coordinates = coordinates[cm == cluster, , drop = FALSE]
      centroid = colMeans(cluster.coordinates)
      apply(cluster.coordinates, 1L, function(point) {
        v = point - centroid
        computeL2Norm(v)
      })
    })
    distances = unlist(distances)
    res = list(
      n_clusters = length(unique(cm)),
      mean_distance = mean(distances),
      norm_n_clusters = normalizeFeature(length(unique(cm)),floor(getNumberOfNodes(x)/5)),
      norm_mean_distance = normalizeFeature(mean(distances), computeL2Norm(c(getWidth(x$coordinates), getHeight(x$coordinates))) / 2)
    )
  } else {
    res = list(
      n_clusters = 0,
      mean_distance = NA,
      norm_n_clusters = 0,
      norm_mean_distance = NA
    )
  }
  prefix = sprintf("cluster_%02ipct", floor(epsilon * 100))
  names(res) = paste(prefix, names(res), sep = "_")
  return(res)
}

# L2-Norm.
#
# @param x[\code{numeric}]\cr
#   Numeric vector.
#
# @return [\code{numeric(1)}].
computeL2Norm = function(x) {
  sqrt(sum(x * x))
}
