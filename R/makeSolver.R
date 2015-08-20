#' @title
#'   Generator for TSP solvers.
#' @description
#'   Construct a TSP solver object.
#'
#' @param solver [\code{character(1)}]\cr
#'   Solver id.
#' @return [\code{TSPSolver}]
makeSolver = function(solver) {
  constructor = getS3method("makeTSPSolver", solver)
  solver = do.call(constructor, list())
  if (isExternalSolver(solver)) {
    solver$bin = solverPaths()[[solver$cl]] #getSolverPath(solver$short.name)
  }
  return(solver)
}

#' @title
#'   Internal generator for TSP solvers.
#'
#' @param cl [\code{character(1)}]\cr
#'   Class name for the TSP solver.
#' @param short.name [\code{character(1)}]\cr
#'   Short name / identifier for the solver.
#' @param name [\code{character(1)}]\cr
#'   Long name of the solver.
#' @param properties [\code{character}]\cr
#'   Character vector of characterizing properties of the learner, e.g., \dQuote{external}
#'   if the solver is called via an external script.
#' @param par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#'   Set of solver parameters with constraints and defaults.
#' @param packages [\code{character}]\cr
#'   Character vector of package names of R packages which are necessary to
#'   run the solver.
#' @param description [\code{character(1)}]\cr
#'   Optional description of the solver or solvers functioning principle.
#' @return [\code{TSPSolver}]
makeTSPSolverInternal = function(cl, short.name, name,
  properties, par.set, packages = NULL, description = NULL) {
  makeS3Obj(
    cl = cl,
    short.name = short.name,
    name = name,
    description = description,
    properties = properties,
    par.set = par.set,
    packages = packages,
    classes = c(cl, "TSPSolver")
  )
}

#' @title
#'   Make TSP solver dispatcher.
#'
#' @return [\code{TSPSolver}]
#' @export
makeTSPSolver = function() {
  UseMethod("makeTSPSolver")
}

#' @title
#'   Check if solver is called via an external binary.
#'
#' @param solver [\code{TSPSolver}]
#' @return [\code{logical(1)}]
isExternalSolver = function(solver) {
  return(hasProperties(solver, "external"))
}

#' @title
#'   Check if learner has certain properties.
#'
#' @param solver [\code{TSPSolver}]\cr
#'   Solver object.
#' @param properties [\code{character}]\cr
#'   Vector of strings.
#' @return [\code{logical(1)}] \code{TRUE} if the solver has all properties, \code{FALSE} otherwise.
#' @export
hasProperties = function(solver, properties) {
  UseMethod("hasProperties")
}

hasProperties.TSPSolver = function(solver, properties) {
  return(isSubset(properties, getSolverProperties(solver)))
}

#' @title
#'   Get the properties/tags of the solver.
#'
#' @param solver [\code{TSPSolver}]\cr
#'   Solver object.
#' @return [\code{character}]
#' @export
getSolverProperties = function(solver) {
  UseMethod("getSolverProperties")
}

getSolverProperties.TSPSolver = function(solver) {
  return(solver$properties)
}

#' @title
#'   Run a TSP solver to solve a problem instance.
#'
#' @param solver [\code{TSPSolver}]\cr
#'   Solver object.
#' @param instance [\code{Network}]\cr
#'   TSP instance to solve.
#' @param solver.pars [\code{list}]\cr
#'   Potentially empty named list of solver parameters passed to the solver.
#' @param ... [any]\cr
#'   Not used.
#' @return [\code{TSPSolverResult}]
#' @export
run = function(solver, instance, solver.pars, ...) {
  UseMethod("run")
}

#' @title
#'   Preprocess instance before running the solver on it.
#'
#' @param solver [\code{TSPSolver}]\cr
#'   Solver object.
#' @param instance [\code{Network}]\cr
#'   Instance to solver.
#' @return [any] Specific to the solver.
#' @export
prepareInstance = function(solver, instance) {
  UseMethod("prepareInstance")
}
