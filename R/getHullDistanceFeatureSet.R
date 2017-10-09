#' Feature: statistics of the distance to the hull edges
#'
#' @template arg_network
#' @param skewness.type [\code{integer(1)}]\cr
#'   Definition for computing the skewness as described in \code{\link[e1071]{skewness}}.
#'   The default is \code{type = 3}.
#' @template arg_include_costs
#' @return [\code{list}]
#' @export
getHullDistanceFeatureSet = function(x, skewness.type = NULL, include.costs = FALSE) {
  assertClass(x, "Network")
  measureTime(expression({
    getHullDistanceFeatureSet2(x)
  }), "hulldist", include.costs)
}

getHullDistanceFeatureSet2 = function(x, skewness.type = NULL) {
  ## we need e1071 for computing the skewness
  requirePackages("e1071", why = "getHullDistanceFeatureSet")

  ## per default use "Type 3" definition skewness in e1071::skewness
  if (is.null(skewness.type)) {
    skewness.type = 3L
  } else {
    assertIntegerish(skewness.type, len = 1L, lower = 1L, upper = 3L)
  }

  ## transform coordinates to [0, 1]^2
  coords = x$coordinates
  coords[,1L] = coords[,1L] - min(coords[,1L])
  coords[,2L] = coords[,2L] - min(coords[,2L])
  
  ## if all coordinates are identical, dividing by the maximum is useless
  if (max(coords) > 0) {
    coords = coords / max(coords)
  }

  ## extract points that form the hull
  hull.points = chull(coords)
  n.hull = length(hull.points)

  ## compute for each point the shortest distance to the hull edges
  hull.distances = vapply(seq_row(coords), function(i) {
    ## if i is one of the hull points, the distance is obviously zero
    if (i %in% hull.points)
      return(0)

    ## coordinates of the current point (X) and all hull points
    X = coords[i,]
    hull.nodes = c(hull.points, hull.points[1L])

    ## compute distances from X to all hull edges
    distances.to.all.hull.edges = vapply(seq_len(n.hull), function(j) {
      # take two neighboring points (A and B) on the hull
      A = coords[hull.nodes[j],]
      B = coords[hull.nodes[j + 1L],]
      ## compute slope (m) and intercept (n) of linear function through A and B
      m = (A[2L] - B[2L]) / (A[1L] - B[1L])
      n = B[2L] - m * B[1L]
      ## compute slope and intercept of orthogonal vector (assuring it runs through X)
      m.orth = -1L / m
      n.orth = X[2L] - m.orth * X[1L]
      ## compute closest point of X on the linear function through A and B --> D
      D.x = (n - n.orth) / (m.orth - m)
      D = c(D.x, m * D.x + n)
      ## compute distance from X to D
      return(sqrt(sum((X - D)^2L)))
    }, double(1L))
    ## return shortest distance from X to any of the hull edges
    return(min(distances.to.all.hull.edges))
  }, double(1L))

  # See Table I in Pihera and Musliu Features
  res = c(
    computeStatisticsOnNumericVector(hull.distances, "hulldist"),
    "hulldist_skew" = e1071::skewness(hull.distances, type = skewness.type)
  )

  return(res)
}

