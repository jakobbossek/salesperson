makeTSPSolver.cheapest_insertion = function() {
  makeTSPSolverInternal(
    cl = "cheapest_insertion",
    short.name = "cheapest_insertion",
    name = "Cheapest Insertion Algorithm for the (euclidean) TSP",
    description = "",
    properties = c("euclidean", "deterministic"),
    packages = "TSP"
  )
}

run.cheapest_insertion = function(solver, instance, solver.pars, ...) {
  return(runSolverFromTSPPackage(solver, instance, solver.pars))
}
