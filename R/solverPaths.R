#' Set pathes to the executables of solvers only available in binary format.
#'
#' In the current version \pkg{salesperson} does not implement the source code
#' of some solvers including lkh and eax, but calls the corresponding compiled
#' executables. Since the CRAN guidelines do not permit shipping binary executables
#' one has the download these by hand and tell the \pkg{salesperson} package about
#' the file path. This is done by the \code{solverPaths} function.
#'
#' @param paths [\code{list}]\cr
#'   Named list where each component indicates the path to the executable of the
#'   corresponding solver. Supported solvers at the momenet are
#'   \describe{
#'     \item{lkh}{Implementation of the Lin-Kernigham heuristic.}
#'     \item{lkh-restart}{Implementation of the Lin-Kernigham heuristic with restart strategy}
#'     \item{eax}{Implementation of the evolutionary EAX algorithm.}
#'     \item{lkh}{Implementation of the evolutionary EAX algorithm with restart strategy.}
#'   }
#' @return [\code{list}] List of pathes. Used internally by \code{runTSPSolver}.
#' @export
solverPaths = function(paths = NULL) {
  defaults = list("lkh" = NULL, "lkh_restart" = NULL, "eax" = NULL, concorde = NULL)
  solver.names = names(defaults)
  if (is.null(paths)) {
    if (is.null(salesperson$paths)) {
      salesperson$paths <- defaults
    }
    return(salesperson$paths)
  } else {
    assertList(paths)
    salesperson$paths <- BBmisc::insert(defaults, paths)
    lapply(solver.names, function(solver.name) {
      path.to.solver = salesperson$paths[[solver.name]]
      if (!is.null(path.to.solver))
        assertFile(path.to.solver, access = "x")
      if (!is.character(path.to.solver)) {
        catf("No path to solver '%s' specified. This solver thus cannot be used.", solver.name)
      }
    })
    invisible(salesperson$paths)
  }
}
