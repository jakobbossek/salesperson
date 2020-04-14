#' Feature: fraction of points near bounding boxplot.
#'
#FIXME: netgen needs a documentation for 'Network' class objects
#' @template arg_network
#' @param distance_fraction [\code{numeric}]\cr
#'   Fraction of distance between min and max coordinates of each dimension
#'   which should points be allowed lie inbetween.
#' @template arg_include_costs
#' @template arg_dots
#' @return [\code{list}]
#' @export
getBoundingBoxFeatureSet = function(x, distance_fraction = 0.1, include.costs = FALSE, ...) {
  assertClass(x, "Network")
  assertNumber(distance_fraction, lower = 0.00001, upper = 0.4999, na.ok = FALSE)
  measureTime(expression({
    getFractionOfPointsNearBoundingBoxCPP(x$coordinates, distance_fraction)
  }), paste("bounding_box", distance_fraction, sep = "_"), include.costs)
}
