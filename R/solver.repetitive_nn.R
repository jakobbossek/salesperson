#' @export
makeTSPSolver.repetitive_nn = function() {
  makeTSPSolverInternal(
    cl = "repetitive_nn",
    short.name = "repetitive_nn",
    name = "Repetitive Nearest-Neighbor Algorithm for the symmetric and asymmetric TSP",
    description = "Computes a Nearest-Neighbor tour for each city and returns the shortest.",
    properties = c("euclidean", "deterministic", "asymmetric"),
    packages = "TSP"
  )
}

#' @export
run.repetitive_nn = function(solver, instance, ...) {
  return(runSolverFromTSPPackage(solver, instance, ...))
}
