# Helper to compute default statistics of numeric vector.
#
# @param x [Network]
#   Network.
# @param type [character(1)]
#   Type of statistics.
# @return [list]
computeStatisticsOnNumericVector = function(x, type, normalize = FALSE) {
  x.mean = mean(x)
  x.sd = sd(x)
  x.min = min(x)
  x.max = max(x)
  x.var.norm = NA

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
  if (normalize) {
    feats$norm_var = NA
    if (x.min > 0) {
      x.harm.mean = 1 / mean(1 / x)
      x.var.upper = (x.max * (x.mean - x.harm.mean) * (x.max - x.mean)) / (x.max - x.harm.mean)
      x.var.lower = (x.min * (x.mean - x.harm.mean) * (x.mean - x.min)) / (x.harm.mean - x.min)
      x.var.norm = normalizeFeature(var(x) * (length(x) - 1) / length(x), x.var.upper, x.var.lower)
      feats$norm_var = x.var.norm
    }
  }
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
