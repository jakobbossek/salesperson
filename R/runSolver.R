#' @title
#'   Apply solver to instance.
#' @description
#'   This is a central function. It expects a solver (either a characterizing string
#'   or a TSPSolver object), a Network to operate on and optional parameter for the
#'   algorithm/solver. It then runs the solver on the instance and returns a solver
#'   result object.
#'
#' @param solver [\code{character(1)} || \code{TSPSolver}]\cr
#'   Solver.
#' @param instance [\code{Network}]\cr
#'   Instance to solve.
#' @param solver.pars [\code{list}]\cr
#'   Named list of parameters for the solver.
#' @param ... [any]
#'   Parameters for the solver.
#' @return [\code{TSPSolverResult}]
#' @export
#FIXME: add possibility to pass path to TSPlib file
#FIXME: add possibility to pass solver object
runSolver = function(solver, instance, solver.pars, ...) {
  if (testString(solver)) {
    solver = makeSolver(solver)
  }
  assertClass(solver, "TSPSolver")
  instance2 = instance
  start.time = proc.time()
  res = run(solver, instance2, solver.pars, ...)
  end.time = as.numeric(proc.time() - start.time)

  res = makeTSPSolverResult(
    #FIXME: multiple cases if instance is network or character
    instance.name = if (testClass(instance, "Network")) instance$name else basename(instance),
    solver = solver$short.name,
    tour.length = res$tour.length,
    tour = res$tour,
    runtime = end.time,
    error = res$error,
    solver.output = res$solver.output
  )
  return(res)
}
