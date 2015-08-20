#' @export
makeTSPSolver.twoopt = function() {
  makeTSPSolverInternal(
    cl = "twoopt",
    short.name = "2-opt",
    name = "2-opt heuristic",
    properties = c("euclidean", "external", "requires.tsplib", "stochastic"),
    par.set = makeParamSet()
  )
}

#' @export
prepareInstance.twoopt = function(solver, instance) {
  prepareInstance.eax(solver, instance)
}

#' @export
# @interface see runTSPSolver
run.twoopt = function(solver, instance, solver.pars, ...) {
  callAustralianSolverInterface(instance, solver.pars, bin = solver$bin, solver = "2OPT")
}
