#' @export
makeTSPSolver.nearest_insertion = function() {
  makeTSPSolverInternal(
    cl = "nearest_insertion",
    short.name = "nearest_insertion",
    name = "Nearest Insertion Algorithm for the (euclidean) TSP",
    description = "",
    properties = c("euclidean", "deterministic"),
    packages = "TSP"
  )
}

#' @export
run.nearest_insertion = function(solver, instance, ...) {
  return(runSolverFromTSPPackage(solver, instance, ...))
}
