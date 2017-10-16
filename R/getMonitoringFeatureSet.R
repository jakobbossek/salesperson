#' Feature: monitoring of algorithm runs.
#'
#' @param times [\code{numeric}]\cr
#'   Times at which logging took place.
#' @param incumbents [\code{numeric}]\cr
#'   Logged best value for each timing.
#' @param lower.bound [\code{numeric(1)}]\cr
#'   Lower bound for the computation of the area under the curve.
#'   Default is 0.
#' @param raw.data [\code{logical(1)}]\cr
#'   Set this to \code{TRUE} if you want the raw data computed on the
#'   trajectory instead of the statistics.
#'   Default is \code{FALSE}.
#' @template arg_include_costs
#' @return [\code{list}] Named list.
#' @export
getMonitoringFeatureSet = function(times, incumbents, lower.bound = 0, include.costs = TRUE, raw.data = FALSE) {
  assertNumeric(times, any.missing = FALSE, all.missing = FALSE)
  assertNumeric(incumbents, any.missing = FALSE, all.missing = FALSE)
  assertNumber(lower.bound, lower = 0, finite = TRUE, na.ok = FALSE)
  assertFlag(raw.data)

  if (length(times) != length(incumbents))
    stopf("Vectors times and incumbents need to be of equal length.")

  res = measureTime(expression({
    .Call("getMonitoringFeatureSetC", as.numeric(times), as.numeric(incumbents), as.numeric(lower.bound), PACKAGE = "salesperson")
  }), "monitoring", include.costs)

  if (raw.data)
    return(res)

  feats_slopes_consecutive = computeStatisticsOnNumericVector(
    res$slopes_consecutive, "monitoring_slopes_consecutive")

  feats_slopes_improvement = computeStatisticsOnNumericVector(
    na.omit(res$slopes_improvement), "monitoring_slopes_improvement")

  feats_vertical_gaps = computeStatisticsOnNumericVector(
    res$vertical_gaps, "monitoring_vertical_gaps")

  feats_plateau_length = computeStatisticsOnNumericVector(
    na.omit(res$plateau_length), "monitoring_plateau_length")

  return(c(feats_slopes_consecutive,
    feats_slopes_improvement, feats_vertical_gaps,
    feats_plateau_length))
}
