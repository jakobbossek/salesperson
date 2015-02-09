#' Feature: Statistics of Nearest-Neighbour distances.
#'
#' @template arg_network
#' @return [\code{list}]
#' @export
#FIXME: C++ implementation throws seg fault error occasionally
getDistanceFeatureSet = function(x) {
    assertClass(x, "Network")
    dist.obj = x$distance.matrix
    dist.mat = as.matrix(dist.obj)
    dist.num = as.numeric(dist.obj)
    n.cities = nrow(dist.mat)
    feat.set = getDistanceFeatureSetCPP(dist.mat, dist.num)
    feat.set = c(feat.set, computeStatisticsOnNumericVector(dist.num, "distance"))
    return(feat.set)
}
