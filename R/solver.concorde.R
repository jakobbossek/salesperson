#' @export
makeTSPSolver.concorde = function() {
  makeTSPSolverInternal(
    cl = "concorde",
    short.name = "concorde",
    name = "Exect CONCORDE solver (sophisticated branch and cut ILP solver)",
    properties = c("euclidean", "deterministic", "exact", "external", "requires.tsplib")
  )
}

#' @export
run.concorde = function(solver, instance, solver.pars, ...) {
  # set concorde path
  return(runSolverFromTSPPackage(solver, instance, solver.pars))
}
