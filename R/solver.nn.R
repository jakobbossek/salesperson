#' @export
makeTSPSolver.nn = function() {
  makeTSPSolverInternal(
    cl = "nn",
    short.name = "nn",
    name = "Nearest-Neighbor Algorithm for the symmetric and asymmetric TSP",
    description = "",
    properties = c("euclidean", "deterministic", "asymmetric"),
    #FIXME: index of the start city is the only parameter here. The default is
    # a random city, but we cannot set that default here, since we do now know
    # the number of nodes
    par.set = makeParamSet(),
    packages = "TSP"
  )
}

#' @export
run.nn = function(solver, instance, solver.pars) {
  return(runSolverFromTSPPackage(solver, instance, solver.pars))
}
