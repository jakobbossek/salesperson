#' @export
makeTSPSolver.repetitive_nn = function() {
  makeTSPSolverInternal(
    cl = "repetitive_nn",
    short.name = "repetitive_nn",
    name = "Repetitive Nearest-Neighbor Algorithm for the symmetric and asymmetric TSP",
    description = "Computes a Nearest-Neighbor tour for each city and returns the shortest.",
    properties = c("euclidean", "deterministic", "asymmetric"),
    #FIXME: index of the start city is the only parameter here. The default is
    # a random city, but we cannot set that default here, since we do now know
    # the number of nodes
    par.set = makeParamSet(),
    packages = "TSP"
  )
}

#' @export
run.repetitive_nn = function(solver, instance, solver.pars, ...) {
  return(runSolverFromTSPPackage(solver, instance, solver.pars))
}
