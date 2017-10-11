# Helper to compute default statistics of numeric vector.
#
# @param x [Network]
#   Network.
# @param type [character(1)]
#   Type of statistics.
# @return [list]
computeStatisticsOnNumericVector = function(x, type, skewness.type = NULL) {
  x.mean = mean(x)
  x.sd = sd(x)
  x.min = min(x)
  x.max = max(x)
  feats = list(
    "mean" = x.mean,
    "sd" = x.sd,
    "var" = var(x),
    "median" = median(x),
    "varcoeff" = x.sd / x.mean,
    "min" = x.min,
    "max" = x.max,
    #FIXME: span in x and y dimension?
    "span" = x.max - x.min
    )
  ## for some features (such as the hull features), we additionally compute the skewness
  if (!is.null(skewness.type)) {
    requirePackages("e1071", why = "computeStatisticsOnNumericVector")
    assertIntegerish(skewness.type, len = 1L, lower = 1L, upper = 3L)
    feats = c(feats, "skew" = e1071::skewness(x, type = skewness.type))
  }
  names(feats) = paste(type, names(feats), sep = "_")
  return(feats)
}
