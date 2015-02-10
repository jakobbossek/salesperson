#' Feature: all features.
#'
#' @template arg_network
#' @param black.list [\code{character}]\cr
#'   Optional black list of feature sets which we do not want to compute. Default
#'   is the empty character vector.
#' @param include.times [\code{logical(1)}]\cr
#'   Include the times needed to compute the specific feature sets as additional
#'   features? Default is \code{FALSE}. Time is measured via \code{proc.time}.
#' @return [\code{list}]
#'   Named list of features.
#' @export
getFeatureSet = function(x, black.list = character(0), include.times = FALSE) {
    assertClass(x, "Network")
    assertSubset(black.list, choices = getAvailableFeatureSets(), empty.ok = TRUE)
    assertFlag(include.times)

    feature.set.names = getAvailableFeatureSets()
    feature.set.names = setdiff(feature.set.names, black.list)

    catf("Using the following feature (sub)sets: %s", collapse(feature.set.names, sep = ", "))

    # now call the funs
    #FIXME: here we currently do not pass any arguments to the separate feature funs.
    # I.e., 'epsilon' for getClusterFeatureSet. Moreover, it is not possible to compute
    # a feature set multiple times with different parameters.
    feats = lapply(feature.set.names, function(feature.set.name) {
        feature.fun = paste("get", feature.set.name, "FeatureSet", sep = "")
        do.call(feature.fun, list(x = x))
    })
    feats = do.call(c, feats)
    return(feats)
}

#FIXME: this needs a better name
#FIXME: x is a rather ugly name. Use instance.set or something similar?
#FIXME: how to best store times? As an numeric attribute? Maybe add logical parameter
# times.are.features which decides whether times are stored as features or separately?
getFeatureSetMultiple = function(x, black.list = c(), include.times = FALSE) {
    assertList(x, types = "Network", any.missing = FALSE, min.len = 1L)
    feats = lapply(x, function(instance) {
        getFeatureSet(instance, black.list, include.times)
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
      "Distance", "Modes", "MST", "NearestNeighbour")
}
