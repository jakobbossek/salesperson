#' Feature: all features.
#'
#' @template arg_network
#' @param black.list [\code{character}]\cr
#'   Optional black list of feature sets which we do not want to compute. Default
#'   is the empty character vector.
#' @param include.costs [\code{logical(1)}]\cr
#'   Include the times needed to compute the specific feature sets as additional
#'   features? Default is \code{FALSE}. Time is measured via \code{proc.time}.
#' @param drop.duplicates [\code{logical(1)}]\cr
#'   Should duplicate nodes be deleted?
#'   Duplicate node coordinates cause some angle features to be NA. Hence,
#'   it is better to drop those duplicates.
#'   Default is \code{TRUE}.
#' @param feature.fun.args [\code{list}]\cr
#'   List of lists. Each component of the list corresponds to one of the feature sets
#'   and the subordered list contains vectors of values for each parameter. Name
#'   the sublists according to \link{getAvailableFeatureSets}.
#' @return [\code{list}]
#'   Named list of features.
#' @examples
#'   x = generateRandomNetwork(n.points = 50L)
#'   # get all features available in salesperson with default parameters
#'   # Note that we always exclude the VRP problem specific VRP features, since
#'   # we are operating on a simple euclidean TSP instance without depots and
#'   # arrival times.
#'   fs = getFeatureSet(x, black.list = "VRP")
#'
#'   # now include the costs of computing (in seconds) for each set of parameters
#'   # as additional features
#'   fs = getFeatureSet(x, black.list = "VRP", include.costs = TRUE)
#'
#'   # compute all but MST and distance features
#'   fs = getFeatureSet(x, black.list = c("VRP", "MST", "Distance"))
#'
#'   # now set user-defined parameter values for cluster
#'   args = list("Cluster" = list("epsilon" = c(0.01, 0.05, 0.1, 0.2, 0.3)))
#'   fs = getFeatureSet(x, black.list = "VRP", feature.fun.args = args)
#' @export
getFeatureSet = function(x, black.list = character(0),
  include.costs = FALSE,
  drop.duplicates = TRUE,
  feature.fun.args = getDefaultFeatureFunArgs()) {
  assertClass(x, "Network")
  assertSubset(black.list, choices = getAvailableFeatureSets(), empty.ok = TRUE)
  assertList(feature.fun.args, types = "list", any.missing = FALSE)
  assertFlag(include.costs)

  feature.set.names = getAvailableFeatureSets()
  feature.set.names = setdiff(feature.set.names, black.list)

  # merge user defined feature-fun args and defaults preferring user-defined
  feature.fun.args = BBmisc::insert(getDefaultFeatureFunArgs(), feature.fun.args)

  feats = lapply(feature.set.names, function(feature.set.name) {
    feature.fun = paste("get", feature.set.name, "FeatureSet", sep = "")
    if (is.null(feature.fun.args[[feature.set.name]])) {
      do.call(feature.fun, list(x = x, include.costs = include.costs, drop.duplicates = drop.duplicates))
    } else {
      feats2 = lapply(feature.fun.args[[feature.set.name]][[1]], function(param) {
        param.list = list(x = x, include.costs = include.costs, drop.duplicates = drop.duplicates)
        param.list = c(param.list, param)
        do.call(feature.fun, param.list)
      })
      do.call(c, feats2)
    }
  })
  feats = do.call(c, feats)
  return(feats)
}

#FIXME: this needs a better name
#FIXME: x is a rather ugly name. Use instance.set or something similar?
#FIXME: how to best store times? As an numeric attribute? Maybe add logical parameter
# times.are.features which decides whether times are stored as features or separately?
getFeatureSetMultiple = function(x, black.list = c(), include.costs = FALSE) {
  assertList(x, types = "Network", any.missing = FALSE, min.len = 1L)
  feats = lapply(x, function(instance) {
    getFeatureSet(instance, black.list, include.costs)
  })
  feats = as.data.frame(do.call(rbind, feats))
  return(feats)
}

#' Available feature (sub)sets.
#'
#' May be used as black list for \code{getFeatureSet}.
#' @export
getAvailableFeatureSets = function() {
  c("Angle", "BoundingBox", "Centroid", "Cluster", "ConvexHull",
    "Distance", "Modes", "MST", "NearestNeighbour", "VRP", "NNG")
}

#' Returns list of parameters defaults for feature computation.
#'
#' @return [\code{list}]
#' @export
getDefaultFeatureFunArgs = function() {
  list(
    "BoundingBox" = list("distance_fraction" = c(0.1, 0.2, 0.3)),
    "Cluster" = list("epsilon" = c(0.01, 0.05, 0.1))
  )
}

