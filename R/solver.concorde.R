#' @export
makeTSPSolver.concorde = function() {
  makeTSPSolverInternal(
    cl = "concorde",
    short.name = "concorde",
    name = "Exect CONCORDE solver (sophisticated branch and cut ILP solver)",
    properties = c("euclidean", "deterministic", "exact", "external", "requires.tsplib"),
    #FIXME: concorde has a huge number of parameters. Are there any interesting ones?
    par.set = makeParamSet()
  )
}

#' @export
run.concorde = function(solver, instance, solver.pars, ...) {
  # set concorde path
  return(runSolverFromTSPPackage(solver, instance, solver.pars))
}
