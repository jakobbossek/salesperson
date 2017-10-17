#' @import BBmisc
#' @import checkmate
#' @import netgen
#' @import Rcpp
#' @import stringr
#' @import TSP
#' @importFrom igraph as.undirected components
#' @importFrom cccd nng
#' @importFrom fpc dbscan
#' @importFrom splancs areapl
#' @importFrom vegan spantree spandepth
#' @importFrom grDevices chull
#' @importFrom stats na.omit median quantile sd var density dist rnorm runif
#' @importFrom utils methods getS3method read.table
#' @useDynLib salesperson
NULL

salesperson = new.env(parent = emptyenv())
