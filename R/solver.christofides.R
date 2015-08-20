#' @export
makeTSPSolver.christofides = function() {
  makeTSPSolverInternal(
    cl = "christofides",
    short.name = "chris",
    name = "Christofides 2/3 approximation",
    properties = c("euclidean", "external", "requires.tsplib", "deterministic"),
    par.set = makeParamSet()
  )
}

#' @export
prepareInstance.christofides = function(solver, instance) {
  prepareInstance.eax(solver, instance)
}

#' @export
# @interface see runTSPSolver
run.christofides = function(solver, instance, control) {
  callAustralianSolverInterface(instance, control, bin = solver$bin, solver = "CHRIS")
}
