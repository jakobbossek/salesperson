# Helper to compute default statistics of numeric vector.
#
# @param x [Network]
#   Network.
# @param type [character(1)]
#   Type of statistics.
# @return [list]
computeStatisticsOnNumericVector = function(x, type) {
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
  names(feats) = paste(type, names(feats), sep = "_")
  return(feats)
}
