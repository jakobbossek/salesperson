#' Feature: Statistics of Nearest-Neighbour distances.
#'
#' @param x [\code{Network}]\cr
#'   Network.
#' @return [\code{list}]
#' @export
#FIXME: C++ implementation throws seg fault error occasionally
getDistanceFeatureSet = function(x) {
    assertClass(x, "Network")
    #FIXME: we need to save dist in Network object
    dist.obj = dist(x$coordinates)
    dist.mat = as.matrix(dist.obj)
    dist.num = as.numeric(dist.obj)
    n.cities = nrow(dist.mat)
    feat.set = getDistanceFeatureSetCPP(dist.mat, dist.num)
    feat.set = c(feat.set, computeStatisticsOnNumericVector(dist.num, "distance"))
    return(feat.set)
}