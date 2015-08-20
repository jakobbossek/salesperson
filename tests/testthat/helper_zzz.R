set.seed(1)

# define some helpers

# Generate simple network with three nodes.
#
# Test network geography:
# (0,1)   (1,1)
# o       o
#
#
#
# o       o
# (0,0)   (1,0)
# @return [Network]
generateSimpleTestNetwork = function() {
  x = list()
  x$coordinates = matrix(c(0, 0, 1, 1, 0, 1, 1, 0), ncol = 2, byrow = TRUE)
  x$distance.matrix = as.matrix(dist(x$coordinates))
  class(x) = "Network"
  return(x)
}


# Check that object is feature list
# @param x [any]
#   Object to check.
# @param feature.set [character(1)]
#   Name of the feature set (for debugging).
expect_feature_list = function(x, feature.set) {
  expect_true(is.list(x))
  for (i in 1:length(x)) {
    expect_true(is.numeric(x[[i]]),
      info = sprintf("Feature '%s' of feature set '%s' is NOT numeric.",
      names(x)[i], feature.set)
    )
  }
}

# Check if TSPSolverResult contains expected structures.
# @param x [any]
#   Object to check.
# @param info [character(1)]
#   Informative text for the exepct_ functions (helpful in loops).
expect_valid_TSPSolverResult = function(x, n.points, info = NULL) {
  expect_true(!is.null(x$tour.length), info = paste0("Tour is NULL in case: ", info))
  expect_true(is.numeric(x$tour.length), info = paste0("Tour is not numeric in case: ", info))
  expect_equal(length(x$tour), n.points, info = paste0("Tour has wrong length in case:", info))
  expect_true(isPermutation(x$tour), info = paste0("Tour is not a permutation in case:, ", info))
  expect_true(all(x$runtime >= 0), info = paste0("Runtime is always greater (or equal) to zero in case: ", info))
  expect_true(is.null(x$error), info = paste0("An error occured in case: ", info)
}
