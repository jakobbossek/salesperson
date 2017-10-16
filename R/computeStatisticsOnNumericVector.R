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
    "span" = x.max - x.min,
    "skew" = skew(x)
  )
  names(feats) = paste(type, names(feats), sep = "_")
  return(feats)
}



#' Computes skewness of a numeric vector (analogous to skewness type 3 of e1071)
#'
#' @param x [\code{numeric}]\cr
#'   Vector used for computing the skewness.
#' @return [\code{numeric(1)}]
skew = function (x) {
  if (any(is.na(x))) {
    return(NA)
  } else {
    # center x
    x = x - mean(x)
    n = length(x)
    y = sqrt(n) * sum(x^3L) / sqrt(sum(x^2L)^(3L))
    return(y * sqrt(((n - 1) / n)^(3L)))
  }
}
