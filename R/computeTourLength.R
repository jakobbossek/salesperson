#FIXME: add comments
computeTourLength = function(x, perm) {
  assertClass(x, "Network")
  n = getNumberOfNodes(x)
  if (!testInteger(perm) && (any(sort(perm) != 1:n))) {
    stopf("Second parameter needs to be a permutation of {1, ..., %i}.", n)
  }
  # close tour
  perm = c(perm, perm[1])
  tour_length = 0

  # compute tour length
  for (i in 1:(length(perm) - 1L)) {
    tour_length = tour_length + x$distance.matrix[perm[i], perm[i + 1L]]
  }
  return(tour_length)
}
