# Compute tour length given a TSP instance and a tour/permutation.
#
# @param x [\code{Network}]\cr
#   TSP instance.
# @param tour [\code{integer}]\cr
#   Permutation of nodes.
# @param round [\code{logical}]\cr
#   Should the distances be rounded? Default is \code{FALSE}. Concorde internally
#   does this.
# @return [\code{numeric(1)}]
# @export
computeTourLength = function(x, tour, round = FALSE) {
  assertClass(x, "Network")
  assertFlag(round)
  n = getNumberOfNodes(x)

  if (!isPermutation(tour, source = seq(n))) {
    stopf("Second parameter needs to be a permutation of {1, ..., %i}.", n)
  }

  # close tour
  tour = c(tour, tour[1L])
  tour_length = 0

  # compute tour length
  for (i in 1:(length(tour) - 1L)) {
    tour_length = tour_length + x$distance.matrix[tour[i], tour[i + 1L]]
    if (round) {
      tour_length = round(tour_length)
    }
  }
  return(tour_length)
}
