#' @title
#' Global mutation operator for EA.
#'
#' @description
#' Selects a fraction of cities at reandom and replaces these by new cities
#' generated uniform at random in [0,1] x [0,1].
#'
#' @param coordinates [\code{matrix}]\cr
#'   Numeric matrix of city coordinates, rows denote cities.
#' @param p [\code{numeric(1)}]\cr
#'   Mutation probability.
#' @return [\code{matrix}]
#'   Numeric matrix of globally mutated city coordinates.
#' @export
mutateUniform = function(coordinates, p) {
  assertMatrix(coordinates, ncols = 2L, any.missing = FALSE, all.missing = FALSE)
  assertNumber(p, lower = 0, upper = 1)
  to.mutate = which(runif(nrow(coordinates)) < p)
  coordinates[to.mutate, ] = matrix(runif(2 * length(to.mutate)), ncol = 2)
  return(coordinates)
}

#' @title
#' Local mutation operator of EA.
#'
#' @description
#' Adds some normal noise to a fraction of city coordinates.
#'
#' @param coordinates [\code{matrix}]\cr
#'   Numeric matrix of city coordinates, rows denote cities.
#' @param p [\code{numeric(1)}]\cr
#'   Mutation probability.
#' @param sigma [\code{numeric(1)}]\cr
#'   Standard deviation of normal noise.
#' @return [\code{matrix}]
#'   Numeric matrix of globally mutated city coordinates.
#' @export
mutateNormal = function(coordinates, p, sigma) {
  assertMatrix(coordinates, ncols = 2L, any.missing = FALSE, all.missing = FALSE)
  assertNumber(p, lower = 0, upper = 1)
  assertNumber(sigma, finite = TRUE, lower = 0.00000001)
  to.mutate = which(runif(nrow(coordinates)) < p)
  # pmin(pmax(...)) used to ensure we stay in bounds
  if (length(to.mutate) > 0) {
    delta = matrix(rnorm(2L * length(to.mutate), sd = sigma), ncol = 2L)
    coordinates[to.mutate, ] = pmin(pmax(coordinates[to.mutate, ] + delta, 0), 1)
  }
  return(coordinates)
}

#' Round instance (points are placed in the center of the grids).
#'
#' @param coordinates [\code{matrix}]\cr
#'   Numeric matrix of city coordinates, rows denote cities.
#' @param n [\code{numeric(1)}]\cr
#'   Number of cells desired, i.e., grid resolution.
#' @return [\code{matrix}]
#'   Numeric matrix of rounded city coordinates.
#' @export
roundToGrid = function(coordinates, n = 100L) {
  assertMatrix(coordinates, ncols = 2L, any.missing = FALSE, all.missing = FALSE)
  n = asInt(n, lower = 1L)

  gr = seq(0, 1, 1 / n)
  roundGridPoint = apply(coordinates, 2L, function(x) {
    sapply(x, function(y) {
      gr[which.min((y - gr)[(y - gr) >= 0])]
    })
  })

   # avoid outliers outside boundary
  helper = function(x) {
    if(all(x != 1))
      y = x + 1 / (2 * n)
    if(all(x == 1))
      y = x - 1 / (2 * n)
    if((x[2] == 1) & (x[1] != 1))
      y = c(x[1] + 1 / (2 * n), x[2] - 1 / (2 * n))
    if((x[1] == 1) & (x[2] != 1))
      y = c(x[1] - 1 / (2 * n), x[2] + 1 / (2 * n))
    return(y)
  }
  t(apply(roundGridPoint, 1L, helper))
}

#' @title
#' Rescale city coordinates.
#'
#' @description
#' Rescale coordinates to [0,1] x [0,1].
#'
#' @param coordinates [\code{matrix}]\cr
#'   Numeric matrix of city coordinates, rows denote cities.
#' @export
rescaleCoordinates = function(coordinates) {
  min = apply(coordinates, 2L, min)
  max = apply(coordinates, 2L, max)
  t((t(coordinates) - min) / (max - min))
}
