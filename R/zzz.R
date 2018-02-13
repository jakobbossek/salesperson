#' @import BBmisc
#' @import checkmate
#' @import Rcpp
#' @import stringr
#' @import TSP
#' @import lhs
#' @import ggplot2
#' @import mvtnorm
#' @import lpSolve
#' @importFrom igraph as.undirected components graph.data.frame V V<- maximum.bipartite.matching
#' @importFrom cccd nng
#' @importFrom fpc dbscan
#' @importFrom splancs areapl
#' @importFrom vegan spantree spandepth
#' @importFrom grDevices chull
#' @importFrom stats na.omit median quantile sd var density dist rnorm runif cmdscale rexp
#' @importFrom utils methods getS3method read.table head read.csv write.table
#' @useDynLib salesperson
NULL

salesperson = new.env(parent = emptyenv())
