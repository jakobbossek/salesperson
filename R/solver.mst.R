#' @export
makeTSPSolver.mst = function() {
  makeTSPSolverInternal(
    cl = "mst",
    short.name = "MST",
    name = "MST (minimum spanning tree) heuristic",
    properties = c("euclidean", "external", "requires.tsplib", "deterministic")
  )
}

#' @export
# @interface see runTSPSolver
run.mst = function(solver, instance, ...) {
  callAustralianSolverInterface(instance, bin = solver$bin, solver = "2APP")
}
