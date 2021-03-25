#' Feature: statistics of the minimum spanning tree edges.
#'
#' @template arg_network
#' @template arg_include_costs
#' @template arg_dots
#' @return [\code{list}]
#' @export
getMSTFeatureSet = function(x, include.costs = FALSE, normalize = FALSE, ...) {
  assertClass(x, "Network")
  measureTime(expression({
    getMSTFeatureSet2(x, normalize = normalize)
  }), "mst", include.costs)
}

getMSTFeatureSet2 = function(x, normalize = FALSE) {
  d = x$distance.matrix
  # compute spanning tree
  span_tree = spantree(d)
  # depths of MST
  span_tree_depth = spandepth(span_tree)
  # distances within MST
  span_tree_dists = span_tree$dist
  
  n.cities = getNumberOfNodes(x)
  L = ceiling(log((n.cities - 1) / 5, base = 4)) + 1
  R = n.cities - (1 + sum(sapply(1:(L - 1), function(x){5 * 4 ** (x - 1)})))
  depth.max.min = if (R > 4 ** (L - 1)) L + 1 else L
  d.max = getDMax(x$coordinates)
  width = getWidth(x$coordinates)
  height = getHeight(x$coordinates)

  statistics.on.mst.depth = computeStatisticsOnNumericVector(span_tree_depth, "mst_depth", normalize = normalize)
  statistics.on.mst.dists = computeStatisticsOnNumericVector(span_tree_dists, "mst_dists", normalize = normalize)
  if (!normalize) {
    res = c(statistics.on.mst.depth, statistics.on.mst.dists)
    res$mst_dists_sum = sum(span_tree_dists) / sum(d)
    return(res)
  }
  res = c(
    statistics.on.mst.depth,
    "mst_depth_norm_mean" = normalizeMSTDepthMean(statistics.on.mst.depth$mst_depth_mean, n.cities, R, L),
    "mst_depth_norm_median" = normalizeFeature(statistics.on.mst.depth$mst_depth_median, if (n.cities %% 4 == 0) n.cities / 4 + 1 / 2 else ceiling(n.cities / 4), 1),
    "mst_depth_norm_max" = normalizeFeature(statistics.on.mst.depth$mst_depth_max, ceiling(n.cities / 2), depth.max.min),
    "mst_depth_norm_span" = normalizeFeature(statistics.on.mst.depth$mst_depth_span, ceiling(n.cities / 2) - 1, depth.max.min - 1),
    statistics.on.mst.dists,
    "mst_dists_norm_mean" = normalizeFeature(statistics.on.mst.dists$mst_dists_mean, d.max, computeL2Norm(c(width, height)) / (n.cities - 1)),
    "mst_dists_norm_median" = normalizeFeature(statistics.on.mst.dists$mst_dists_median, d.max),
    "mst_dists_norm_min" = normalizeFeature(statistics.on.mst.dists$mst_dists_min, d.max),
    "mst_dists_norm_max" = normalizeFeature(statistics.on.mst.dists$mst_dists_max, computeL2Norm(c(width, height)), computeL2Norm(c(width, height)) / (n.cities - 1)),
    "mst_dists_norm_span" = normalizeFeature(statistics.on.mst.dists$mst_dists_span, computeL2Norm(c(width, height)))
  )

  res$mst_dists_sum = sum(span_tree_dists) / sum(d)
  return(res)
}

normalizeMSTDepthMean = function(value, n, R, L){
  if (R < 0){
    L = L - 1
    R = n - (1 + sum(sapply(1:(L - 1), function(x){5 * 4 ** (x - 1)})))
  }
  val.min = (L + sum(sapply(1:(L - 1), function(x){5 * 4 ** (x - 1) * (L - x)})) + sumOfAllDepthGrowth(R, L)) / n
  value.norm = normalizeFeature(value , if (n %% 2 == 1) ceiling(n / 2) ** 2 / n else n / 2 * (n / 2 + 1) / n, val.min)
  return(value.norm)
}

sumOfAllDepthGrowth = function(r,l){
  if (l == 1){
    return(if(r > 1) r + 1 else r)
  }
  return(r + sumOfAllDepthGrowth(ceiling(r / 4), l - 1))
}