# ADAPTERS to other R packages and other libraries

#' Adapter/interface to the \code{\link[TSP]{solve_TSP}} method.
#'
#' @param solver [\code{character(1)}]\cr
#'    Name of the solver.
#' @param instance [\code{\link[netgen]{Network}}]\cr
#'    Netgen network.
#' @param solver.pars [\code{list}]\cr
#'    Named list of solver parameters.
#' @return [\code{list}]
runSolverFromTSPPackage = function(solver, instance, solver.pars = NULL) {
  requirePackages("TSP", why = "runSolverFromTSPPackage")
  # convert to TSP file format ...
  instance2 = TSP(instance$distance.matrix)
  # ... and solve
  res = TSP::solve_TSP(instance2, method = solver$cl)
  return(
    list(
      "tour" = as.integer(res),
      "tour.length" = attr(res, "tour_length"), # TSP::tour_length(res) # hae? not working
      error = NULL,
      solver.output = NULL
    )
  )
}
