#' Feature: statistics of angles between nodes and their two nearest neighbors.
#'
#' @template arg_network
#' @param drop.duplicates [\code{logical(1)}]\cr
#'   Should duplicated node coordinates be dropped?
#'   Default is \code{FALSE}.
#' @template arg_include_costs
#' @return [\code{list}]
#' @export
getAngleFeatureSet = function(x, drop.duplicates = FALSE, include.costs = FALSE) {
  assertClass(x, "Network")
  assertFlag(drop.duplicates)
  coordinates = x$coordinates
  distance.matrix = as.matrix(x$distance.matrix)
  if (drop.duplicates) {
    idx.dups = which(duplicated(coordinates))
    if (length(idx.dups) > 0L) {
      warningf("Removing duplicated node coordinates.")
      coordinates = coordinates[-idx.dups, , drop = FALSE]
      distance.matrix = distance.matrix[-idx.dups, ]
      distance.matrix = distance.matrix[, -idx.dups]
    }
  }
  measureTime(expression({
    angles = getAnglesToNearestNeighborsCPP(coordinates, distance.matrix)
    computeStatisticsOnNumericVector(angles, "angle")
  }), "angle", include.costs)
}
