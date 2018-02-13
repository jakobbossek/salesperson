# Removes the file extension of a given filename.
#
# @param filename [character(1)]
#   Filename.
# @return Truncated filename, i.e., filename without last file extension.
removeFileExtension = function(filename) {
  # split by path
  x = str_split(filename, pattern = "\\.")
  # since we only provide a single filename, we select the first (and only) list element
  x = x[[1]]
  n = length(x)
  collapse(x[-n], sep = ".")
}

# Convert netgen object to tspmeta TSP instance.
#
# @param x [Network]
#   Network.
# @return [tsp_instance]
netgenToTSPmeta = function(x) {
  structure(list(coords = x$coordinates, dists = x$distance.matrix),
    class = c("tsp_instance_euclidean_coords", "tsp_instance_symmetric", "tsp_instance"))
}

# Check if vector contains permutation of number 1:n
#
# @param x [\code{numeric}]\cr
#   Vector to check.
# @param source [\code{numeric}]\cr
#   Vector which we want to compare with. Default is 1, ..., length(x).
# @return [\code{logical(1)}]
isPermutation = function(x, source = seq(length(x))) {
  return(all(source == sort(x)))
}

# Component-wise convex combination of two matrizes.
#
# @param coords1 [matrix]
#   First matrix.
# @param coords2 [matrix]
#   Second matrix.
# @param alpha [numeric(1)]
#   Coefficient for convex combination.
# @return [matrix]
makeConvexCombination = function(coords1, coords2, alpha) {
  alpha * coords1 + (1 - alpha) * coords2
}

# Computes the euclidean distance between two vectors.
#
# @param x [numeric]
#   First numeric vector.
# @param y [numeric]
#   Second numeric vector.
# @return [numeric(1)]
euklideanDistance = function(x, y) {
  sqrt(crossprod(x - y))
}

# Computes the euclidean distance between a vector a a matrix.
#
# @param x [numeric]
#   First numeric vector.
# @param y [numeric]
#   Numeric matrix.
# @return [numeric]
euklideanDistances = function(x, y) {
  assertNumeric(x, min.len = 2L, any.missing = FALSE, all.missing = FALSE)
  assertMatrix(y, any.missing = FALSE, all.missing = FALSE, ncols = length(x))
  sapply(1:nrow(y), function(i) {
    euklideanDistance(x, y[i, ])
  })
}

# Generate random string.
#
# @param length [integer(1)]
#   Desired length of the string.
# @return [character(1)]
generateRandomString = function(length = 10L) {
  collapse(sample(c(0:9, letters, LETTERS), size = length, replace = TRUE), sep = "")
}

# Generate a (partially) random name.
#
# @param n.points [integer(1)]
#   Number of points.
# @param n.dim [integer(2)]
#   Number of dimensions.
# @param n.cluster [integer(1)]
#   Number of clusters. Default is 1.
# @return [character(1)]
generateName = function(n.points, n.dim, n.cluster = 1L) {
  paste(
    "n", n.points,
    "cl", n.cluster,
    "d", n.dim,
    generateRandomString(),
    sep = "_"
  )
}
