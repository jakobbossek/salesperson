#' Feature: modes features.
#'
#' @template arg_network
#' @template arg_include_costs
#' @template arg_dots
#' @return [\code{list}]
#' @export
getModesFeatureSet = function(x, include.costs = FALSE, normalize = FALSE, ...) {
  assertClass(x, "Network")
  measureTime(expression({
    getModesFeatureSet2(x, normalize = normalize)
  }), "modes", include.costs)
}

getModesFeatureSet2 = function(x, normalize) {
  distances = as.numeric(x$distance.matrix)

  intdens = function(a, b) {
    mean(y[a:b]) * (d$x[b] - d$x[a])
  }

  d = density(distances)
  y = d$y
  n = length(y)

  minidx = c(1L, which(y[2:(n - 1)] < y[1:(n - 2)] & y[2:(n - 1)] < y[3:n]), n + 1)
  modemass = sapply(1:(length(minidx) - 1L), function(i) {
    intdens(minidx[i], minidx[i + 1] - 1)
  })
  modes.number = sum(modemass > 0.01)
  if (!normalize) {
    return(list(modes_number = modes.number))
  }
  list(
    modes_number = normalizeFeature(modes.number, 99)
  )
}
