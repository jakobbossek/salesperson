#' @export
makeTSPSolver.farthest_insertion = function() {
  makeTSPSolverInternal(
    cl = "farthest_insertion",
    short.name = "farthest_insertion",
    name = "Farthest Insertion Algorithm for the (euclidean) TSP",
    description = "",
    properties = c("euclidean", "deterministic"),
    par.set = makeParamSet(),
    packages = "TSP"
  )
}

#' @export
run.farthest_insertion = function(solver, instance, solver.pars) {
  return(runSolverFromTSPPackage(solver, instance, solver.pars))
}
