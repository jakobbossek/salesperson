#' Returns all currently implemented solver names.
#'
#' @return [\code{character}]
#' @export
getAvailableSolverNames = function() {
  solvers = as.character(methods("makeTSPSolver"))
  solvers = gsub("makeTSPSolver.", "", solvers, fixed = TRUE)
  return(solvers)
}
