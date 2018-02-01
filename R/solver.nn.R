#' @export
makeTSPSolver.nn = function() {
  makeTSPSolverInternal(
    cl = "nn",
    short.name = "nn",
    name = "Nearest-Neighbor Algorithm for the symmetric and asymmetric TSP",
    description = "",
    properties = c("euclidean", "deterministic", "asymmetric"),
    packages = "TSP"
  )
}

#' @export
run.nn = function(solver, instance, ...) {
  return(runSolverFromTSPPackage(solver, instance, ...))
}
