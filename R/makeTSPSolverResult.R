#' @title
#' TSP-Solver result object.
#'
#' @description
#' Contains information returned by a solver on a specific problem instance.
#' Includes the following elements:
#' \describe{
#'   \item{instance.name}{Name of the instance solved.}
#'   \item{solver}{Solver name used to solve the instance.}
#'   \item{tour.length}{Tour length}
#'   \item{tour}{Permutation of the nodes.}
#'   \item{runtime}{Running time measured via \code{proc.time}}
#'   \item{error}{Error message, which occured during optimization.}
#' }
#' @rdname TSPSolverResult
#' @name TSPSolverResult
NULL

# Generator for TSPSolverResult objects.
#
# @param instance.name [\code{character(1)}]\cr
#   Instance name.
# @param solver [\code{character(1)}]\cr
#   Solver used to solve instance.
# @param tour.length [\code{numeric(1)} | \code{NA}]\cr
#   Length of the shortest tour found or NA if unknown.
# @param tour [\code{integer}]\cr
#   Tour, i.e., permutation of the node IDs.
# @param runtime [\code{numeric(1)}]\cr
#   Actual runtime of the solver on the instance.
# @param error [\code{character(1)}]\cr
#   Error message in case of solver failing on instance.
# @param solver.output [\code{character(1)}]\cr
#   Raw stdout/stderr output of the solver.
# @return [\code{TSPSolverResult}]
#   Result object.
makeTSPSolverResult = function(
  instance.name,
  solver,
  tour.length = NA, tour = NA,
  runtime = NA, error = NULL,
  solver.output = NULL) {
  assertCharacter(instance.name, len = 1L)
  assertCharacter(solver, len = 1L)
  !is.na(tour.length) && assertNumber(tour.length, na.ok = FALSE)
  !is.na(tour) && assertInteger(tour, min.len = 1L, lower = 1L, any.missing = FALSE)
  !is.na(runtime) && assertNumeric(runtime, len = 5L, any.missing = FALSE)
  if (!isPermutation(tour)) {
    stopf("Passed tour is not a permutation of the nodes!")
  }
  makeS3Obj(
    instance.name = instance.name,
    solver = solver,
    tour.length = tour.length,
    tour = tour,
    runtime = runtime,
    error = error,
    solver.output = solver.output,
    classes = "TSPSolverResult"
  )
}

#' Print TSPSolverResult to stdout.
#'
#' @param x [\code{TSPSolverResult}]\cr
#'   Result object.
#' @param ... [any]\cr
#'   Not used.
#' @export
print.TSPSolverResult = function(x, ...) {
  if (!is.null(x$error)) {
    catf("Instance '%s' could not be solved due to an error!", x$instance.name)
    catf("Error message: %s", as.character(x$error))
  } else {
    catf("Solved instance '%s' successfully!", x$instance.name)
    catf("Used solver:  %s", toupper(x$solver))
    catf("Elapsed time: %.2f [seconds]", x$runtime[3L])
    catf("Tour length:  %.2f", x$tour.length)
    max.idx = min(length(x$tour), 10L)
    catf("Head of tour: %s", paste(collapse(x$tour[1:max.idx], sep = ", "), ", ...", sep = ""))
  }
}
