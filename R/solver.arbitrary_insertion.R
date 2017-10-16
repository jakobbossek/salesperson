makeTSPSolver.arbitrary_insertion = function() {
  makeTSPSolverInternal(
    cl = "arbitrary_insertion",
    short.name = "arbitrary_insertion",
    name = "Arbitrary Insertion Algorithm for the (euclidean) TSP",
    description = "",
    properties = c("euclidean", "deterministic"),
    packages = "TSP"
  )
}

run.arbitrary_insertion = function(solver, instance, solver.pars, ...) {
  return(runSolverFromTSPPackage(solver, instance, solver.pars))
}
