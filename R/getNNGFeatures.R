#' Feature: statistics of the nearest neighbour graph
#'
#' @template arg_network
#' @param ks [\code{integer}]\cr
#'   List of \eqn{k} values for \eqn{k}-Nearest-Neighbour-Graph computation.
#' @template arg_include_costs
#' @template arg_normalize
#' @template arg_dots
#' @return [\code{list}]
#' @export
getNNGFeatureSet = function(x, ks = NULL, include.costs = FALSE, normalize = FALSE, ...) {
  assertClass(x, "Network")
  measureTime(expression({
    getNNGFeatureSet2(x, ks = ks, normalize = normalize)
  }), "nng", include.costs)
}

getNNGFeatureSet2 = function(x, ks = NULL, normalize = FALSE) {
  n = x$number.of.nodes

  if (is.null(ks))
    ks = c(3, 5, 7, floor(n^(1/3)), floor(2 * n^(1/3)), floor(0.5 * n^(0.5)), floor(n^(0.5)))

  requirePackages(c("igraph", "cccd"), why = "getNNGFeatureSet")

  res = lapply(ks, function(k)
    getkNNGFeatureSet(x = x, k = k, kchar = as.character(k), normalize = normalize))
  do.call(c, res)
}

getkNNGFeatureSet = function(x, k, kchar, normalize = FALSE) {
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
  stats.on.weak = computeStatisticsOnNumericVector(comps.weak$csize, "weak_components", normalize = normalize)
  stats.on.strong = computeStatisticsOnNumericVector(comps.strong$csize, "strong_components", normalize = normalize)
  if (!normalize) {
    res = c(
      n_weak = comps.weak$no,
      n_strong = comps.strong$no,
      strong_weak_ratio = comps.strong$no / comps.weak$no,
      stats.on.weak,
      stats.on.strong
    )
    kchar = paste0("nng_", kchar, "_")
    names(res) = paste0(kchar, names(res))
    return(res)
  } else {
  stats.on.weak.norm = c(
    "weak_components_mean" = normalizeFeature(stats.on.weak$weak_components_mean, n, n / floor(n/(k + 1))),
    "weak_components_sd" = NA,
    "weak_components_var" = stats.on.weak$weak_components_norm_var,
    "weak_components_median" = normalizeFeature(stats.on.weak$weak_components_median, n, k + 1),
    "weak_components_varcoeff" = NA,
    "weak_components_min" = normalizeFeature(stats.on.weak$weak_components_min, n, k + 1),
    "weak_components_max" = normalizeFeature(stats.on.weak$weak_components_max, n, k + 1 + ceiling((n %% (k + 1)) / floor(n / (k + 1)))),
    "weak_components_span" = normalizeFeature(stats.on.weak$weak_components_span, n - 2 * (k + 1)),
    "weak_components_skew" = NA
  )
  stats.on.strong.norm = c(
    "strong_components_mean" = normalizeFeature(stats.on.strong$strong_components_mean, n, n / (n - k)),
    "strong_components_sd" = NA,
    "strong_components_var" = stats.on.strong$strong_components_norm_var,
    "strong_components_median" = normalizeFeature(stats.on.strong$strong_components_median, n, 1),
    "strong_components_varcoeff" = NA,
    "strong_components_min" = normalizeFeature(stats.on.strong$strong_components_min, n, 1),
    "strong_components_max" = normalizeFeature(stats.on.strong$strong_components_max, n, k + 1),
    "strong_components_span" = normalizeFeature(stats.on.strong$strong_components_span, n - 2),
    "strong_components_skew" = NA
  )
    res = c(
      n_weak = normalizeFeature(comps.weak$no, floor(n / (k + 1)), 1),
      n_strong = normalizeFeature(comps.strong$no, n - k, 1),
      strong_weak_ratio = NA,
      stats.on.weak.norm,
      stats.on.strong.norm
    )
  }

  kchar = paste0("nng_", kchar, "_")
  names(res) = paste0(kchar, names(res))
  return(res)
}
