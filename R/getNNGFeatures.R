#' Feature: statistics of the nearest neighbour graph
#'
#' @template arg_network
#' @param ks [\code{integer}]\cr
#'   List of \eqn{k} values for \eqn{k}-Nearest-Neighbour-Graph computation.
#' @template arg_include_costs
#' @template arg_dots
#' @return [\code{list}]
#' @export
getNNGFeatureSet = function(x, ks = NULL, include.costs = FALSE, ...) {
  assertClass(x, "Network")
  measureTime(expression({
    getNNGFeatureSet2(x, ks = ks)
  }), "nng", include.costs)
}

getNNGFeatureSet2 = function(x, ks = NULL) {
  n = x$number.of.nodes

  if (is.null(ks))
    ks = c(3, 5, 7, floor(n^(1/3)), floor(2 * n^(1/3)), floor(0.5 * n^(0.5)), floor(n^(0.5)))

  requirePackages(c("igraph", "cccd"), why = "getNNGFeatureSet")

  res = lapply(ks, function(k)
    getkNNGFeatureSet(x = x, k = k, kchar = as.character(k)))
  do.call(c, res)
}

getkNNGFeatureSet = function(x, k, kchar) {
  # compute directed Nearest-Neighbor-Graph
  # Here we use FNN (fast neighbor computation)
  nng.dir = cccd::nng(x$coordinates, k = k, use.fnn = TRUE, algorithm = "kd_tree")

  # ... and its undirected version
  nng.undir = igraph::as.undirected(nng.dir, mode = "collapse")
  n = getNumberOfNodes(x)

  # get components
  comps.strong = igraph::components(nng.dir, mode = "strong")
  comps.weak = igraph::components(nng.undir, mode = "weak")

  # See Table I in Pihera and Musliu Features
  stats.on.weak = computeStatisticsOnNumericVector(comps.weak$csize, "weak_components")
  stats.on.weak.norm = lapply(stats.on.weak, function(f) f / n)
  names(stats.on.weak.norm) = paste(names(stats.on.weak.norm), "norm", sep = "_")
  stats.on.strong = computeStatisticsOnNumericVector(comps.strong$csize, "strong_components")
  stats.on.strong.norm = lapply(stats.on.strong, function(f) f / n)
  names(stats.on.strong.norm) = paste(names(stats.on.strong.norm), "norm", sep = "_")

  res = c(
    n_weak = comps.weak$no,
    n_norm_weak = comps.weak$no / n,
    n_strong = comps.strong$no,
    n_norm_strong = comps.strong$no / n,
    strong_weak_ratio = comps.strong$no / comps.weak$no,
    stats.on.weak,
    stats.on.weak.norm,
    stats.on.strong,
    stats.on.strong.norm
  )

  kchar = paste0("nng_", kchar, "_")
  names(res) = paste0(kchar, names(res))
  return(res)
}
