#' @title Apply TSP solver to instance.
#'
#' @description
#' This is a central function. It expects a solver (either a characterizing string
#' or a TSPSolver object), a Network to operate on and optional parameter for the
#' algorithm/solver. It then runs the solver on the instance and returns a solver
#' result object.
#'
#' @keywords optimize
#'
#' @param solver [\code{character(1)} || \code{TSPSolver}]\cr
#'   Either a string representation of the solver or a \code{TSPSolver} object.
#' @param instance [\code{Network} | \code{character(1)}]\cr
#'   Instance to solve. Either a \code{Network} or a file path to the
#'   instance in TSPlib format.
#' @param solver.pars [\code{list}]\cr
#'   Named list of parameters for the solver.
#' @param solver.path [\code{character(1) | NULL}]\cr
#'   Path to external binary. Only relevant if global path is not
#'   set (see \code{\link{solverPaths}} function for details).
#'   Default is \code{NULL}.
#' @param ... [any]
#'   Alternative possibility to pass down parameters to the solver. Parameters
#'   passed this way take precedence over parameters passed via \code{solver.pars}.
#' @return [\code{TSPSolverResult}]
#' @export
runSolver = function(solver, instance, solver.pars = list(), solver.path = NULL, ...) {
  # create solver object
  if (testString(solver)) {
    solver.name = solver
    solver = try({makeSolver(solver)}, silent = TRUE)
    if (BBmisc::is.error(solver))
      stopf("There is no solver '%s'.", solver.name)
  }
  assertClass(solver, "TSPSolver")

  # make sure binary for external solvers
  if (isExternalSolver(solver)) {
    if (is.null(solver$bin) & is.null(solver.path))
      stopf("'%s' is an external solver. Please provide path to binary
        via solver.path argument or solverPaths function.", toupper(solver))
    if (!is.null(solver.path)) {
      assertFile(solver.path, access = "x")
      solver$bin = solver.path
    }
  }

  # gather parameters
  solver.pars = BBmisc::insert(solver.pars, list(...))

  #FIXME: handle path to instance and network instance cases

  # run solver
  instance2 = instance
  start.time = proc.time()
  run.fun = getS3method("run", solver)
  res = do.call(run.fun, c(list(solver = solver, instance = instance2), solver.pars))
  runtime = as.numeric(proc.time() - start.time)

  makeTSPSolverResult(
    instance.name = if (testClass(instance, "Network")) instance$name else basename(instance),
    solver = solver$short.name,
    solver.id = if (!is.null(res$solver.id)) res$solver.id else NA,
    tour.length = res$tour.length,
    tour = res$tour,
    runtime = BBmisc::coalesce(res$runtime, runtime),
    error = res$error,
    solver.output = res$solver.output,
    trajectory = res$trajectory
  )
}
