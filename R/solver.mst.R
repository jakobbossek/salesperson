#' @export
makeTSPSolver.mst = function() {
  makeTSPSolverInternal(
    cl = "mst",
    short.name = "MST",
    name = "MST (minimum spanning tree) heuristic",
    properties = c("euclidean", "external", "requires.tsplib", "deterministic"),
    par.set = makeParamSet()
  )
}

#' @export
prepareInstance.mst = function(solver, instance) {
  prepareInstance.eax(solver, instance)
}

#' @export
# @interface see runTSPSolver
run.mst = function(solver, instance, solver.pars, ...) {
  callAustralianSolverInterface(instance, solver.pars, bin = solver$bin, solver = "2APP")
}
