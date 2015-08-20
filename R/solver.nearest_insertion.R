#' @export
makeTSPSolver.nearest_insertion = function() {
  makeTSPSolverInternal(
    cl = "nearest_insertion",
    short.name = "nearest_insertion",
    name = "Nearest Insertion Algorithm for the (euclidean) TSP",
    description = "",
    properties = c("euclidean", "deterministic"),
    par.set = makeParamSet(),
    packages = "TSP"
  )
}

#' @export
run.nearest_insertion = function(solver, instance, solver.pars, ...) {
  return(runSolverFromTSPPackage(solver, instance, solver.pars))
}
