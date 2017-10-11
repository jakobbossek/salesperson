#' Feature: number of points on convex hull and spanning area.
#'
#' @template arg_network
#' @param feature.set [\code{character}]\cr
#'   Subset of hull feature sets that should be computed. Possible choices are
#'   \code{"points"} (ratio of points defining the convex hull), \code{"area"}
#'   (area of the convex hull), \code{"edges"} (summary statistics of the
#'   lengths of the hull edges) and \code{"dists"} (summary statistics of the
#'   distances of each city to its closest edge of the hull).
#'   Per default (\code{NULL}), all of the four previously listed feature sets will
#'   be computed.
#' @param skewness.type [\code{integer(1)}]\cr
#'   Definition for computing the skewness as described in \code{\link[e1071]{skewness}}.
#'   The default is \code{type = 3L}.
#' @template arg_include_costs
#' @return [\code{list}]
#' @export
getConvexHullFeatureSet = function(x, feature.set = NULL, skewness.type = 3L, include.costs = FALSE) {
  assertClass(x, "Network")
  assertSubset(feature.set, choices = c("points", "area", "edges", "dists"))
  if (is.null(feature.set))
    feature.set = c("points", "area", "edges", "dists")

  ## initialize by getting points on the convex hull
  hull.list = measureTime(expression({
    getPointsOnConvexHull(x)
  }), "hull_initialization", include.costs)

  
  ## add the initialization costs in case we want to include the costs
  if (include.costs)
    feats = list(hull_initialization_costs = hull.list$hull_initialization_costs)
  else
    feats = NULL

  ## compute the ratio of points defining the hull
  if ("points" %in% feature.set) {
    feats = c(
      feats,
      measureTime(expression({
        getConvexHullPointRatioFeatureSet(hull.list = hull.list)
      }), "hull_points", include.costs)
    ) 
  }

  ## compute the area of the convex hull
  if ("area" %in% feature.set) {
    feats = c(
      feats,
      measureTime(expression({
        getConvexHullAreaFeatureSet(hull.list = hull.list)
      }), "hull_area", include.costs)
    ) 
  }

  ## compute statistics based on the hull's edges
  if ("edges" %in% feature.set) {
    feats = c(
      feats,
      measureTime(expression({
        getConvexHullEdgeFeatureSet(x = x, hull.list = hull.list, skewness.type = skewness.type)
      }), "hull_edges", include.costs)
    )
  }

  ## compute statistics based on the (per city) closest distance to the hull
  if ("dists" %in% feature.set) {
    feats = c(
      feats,
      measureTime(expression({
        getConvexHullDistanceFeatureSet(x = x, hull.list = hull.list, skewness.type = skewness.type)
      }), "hull_dists", include.costs)
    )
  }

  return(feats)
}


## initialize hull features by extracting the nodes that define the respective hull
getPointsOnConvexHull = function(x) {
  coordinates = x$coordinates
  hull = chull(coordinates[, 1L], coordinates[, 2L])
  list(
    coordinates = coordinates,
    hull = hull
  )
}


## ratio of nodes that define the convex hull
getConvexHullPointRatioFeatureSet = function(hull.list) {
  list(
    hull_points_ratio = length(hull.list$hull) / nrow(hull.list$coordinates)
  )
}

## area of the convex hull
getConvexHullAreaFeatureSet = function(hull.list) {
  list(
    hull_area = splancs::areapl(hull.list$coordinates[hull.list$hull, ])
  )
}

## summary statistics of the lengths of the hull's *edges*
getConvexHullEdgeFeatureSet = function(x, hull.list, skewness.type) {
  requirePackages("e1071", why = "getConvexHullFeatureSet: edges")
  assertIntegerish(skewness.type, len = 1L, lower = 1L, upper = 3L, any.missing = FALSE)

  ## city indices for round trip along the hull
  hull.tour = c(hull.list$hull, hull.list$hull[1L])

  ## extract the distances of the hull edges
  hull.edges = unlist(lapply(seq_along(hull.list$hull), function(i) {
    x$distance.matrix[hull.tour[i], hull.tour[i + 1L]]
  }))

  # See Table I in Pihera and Musliu Features
  computeStatisticsOnNumericVector(hull.edges, "hull_edges", skewness.type = skewness.type)
}


## summary statistics of *distances* from all points to the closest edge on the hull
getConvexHullDistanceFeatureSet = function(x, hull.list, skewness.type) {
  requirePackages("e1071", why = "getConvexHullFeatureSet: dists")
  assertIntegerish(skewness.type, len = 1L, lower = 1L, upper = 3L, any.missing = FALSE)

  ## city indices for round trip along the hull
  hull.tour = c(hull.list$hull, hull.list$hull[1L])
  n.hull = length(hull.list$hull)

  coords = x$coordinates

  ## compute for each point the shortest distance to the hull edges
  hull.distances = vapply(seq_row(coords), function(i) {
    ## if i is already one of the hull points, the distance is obviously zero
    if (i %in% hull.list$hull)
      return(0L)

    ## coordinates of the current point (X)
    X = coords[i,]

    ## compute distances from X to all hull edges
    distances.to.all.hull.edges = vapply(seq_len(n.hull), function(j) {
      # take two neighboring points (A and B) on the hull
      A = coords[hull.tour[j],]
      B = coords[hull.tour[j + 1L],]
      if (A[2] == B[2]) {
        ## if A and B are on the same horizontal line, the closest point of X on AB
        ## (denoted D) inherits the x-position of X and the y-position of A (resp. B)
        D = c(X[1], A[2])
      } else if (A[1L] == B[1L]) {
        ## if A and B are on the same vertical line, the closest point of X on AB
        ## (denoted D) inherits the x-position of A (resp. B) and the y-position of X
        D = c(A[1], X[2])
      } else {
        ## compute slope (m) and intercept (n) of linear function (y = m * x + n) running through A and B
        m = (A[2L] - B[2L]) / (A[1L] - B[1L])
        n = B[2L] - m * B[1L]
        ## compute slope and intercept of orthogonal vector (assuring it runs through X)
        m.orth = -1L / m
        n.orth = X[2L] - m.orth * X[1L]
        ## compute closest point of X on the linear function through A and B --> D
        D.x = (n - n.orth) / (m.orth - m)
        D = c(D.x, m * D.x + n)
      }
      ## compute distance from X to D
      return(sqrt(sum((X - D)^2L)))
    }, double(1L))
    ## return shortest distance from X to any of the hull edges
    return(min(distances.to.all.hull.edges))
  }, double(1L))

  ## See Table I in Pihera and Musliu Features
  res = computeStatisticsOnNumericVector(hull.distances, "hull_dists", skewness.type = skewness.type)
  
  ## in addition to Pihera and Musliu:
  ## ratio of points that are located on the hull (but do not necessarily define the hull)
  res = c(res, "hull_dists_point_ratio" = mean(hull.distances == 0))

  return(res)
}
