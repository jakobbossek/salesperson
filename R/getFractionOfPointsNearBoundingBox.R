#' Feature: fraction of points near bounding boxplot.
#'
#' @param x [\code{\link[netgen]{Network}}]\cr
#'   Network.
#' @param distance_fraction [\code{numeric}]\cr
#'   Fraction of distance between min and max coordinates of each dimension
#'   which should points be allowed lie inbetween.
#' @return [\code{list}]
#' @export
getFractionOfPointsNearBoundingBox = function(x, distance_fraction = 0.1) {
    assertClass(x, "Network")
    assertNumber(distance_fraction, min = 0.00001, upper = 0.4999, na.ok = FALSE)
    return(getFractionOfPointsNearBoundingBoxCPP(x$coordinates, distance_fraction))
}