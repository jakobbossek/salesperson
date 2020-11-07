#' Feature: modes features.
#'
#' @template arg_network
#' @template arg_include_costs
#' @template arg_dots
#' @return [\code{list}]
#' @export
getModesFeatureSet = function(x, include.costs = FALSE, ...) {
  assertClass(x, "Network")
  measureTime(expression({
    getModesFeatureSet2(x)
  }), "modes", include.costs)
}

getModesFeatureSet2 = function(x) {
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
  list(modes_number = modes.number,
       modes_norm_number = normalizeFeature(modes.number, 99)
  )
}
