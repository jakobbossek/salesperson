#' @export
makeTSPSolver.twoopt = function() {
  makeTSPSolverInternal(
    cl = "twoopt",
    short.name = "2-opt",
    name = "2-opt heuristic",
    properties = c("euclidean", "external", "requires.tsplib", "stochastic")
  )
}

#' @export
# @interface see runTSPSolver
run.twoopt = function(solver, instance, ...) {
  callAustralianSolverInterface(instance, bin = solver$bin, solver = "2OPT")
}
