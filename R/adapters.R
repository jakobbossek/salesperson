# ADAPTERS to other R packages and other libraries

#' Adapter/interface to the \code{\link[TSP]{solve_TSP}} method.
#'
#' @param solver [\code{character(1)}]\cr
#'    Name of the solver.
#' @param instance [\code{Network}]\cr
#'    Netgen network.
#' @param solver.pars [\code{list}]\cr
#'    Named list of solver parameters.
#' @return [\code{list}]
runSolverFromTSPPackage = function(solver, instance, solver.pars = NULL) {
  requirePackages("TSP", why = "runSolverFromTSPPackage")
  # convert to TSP file format ...
  instance2 = TSP(instance$distance.matrix)
  # ... and solve
  if (solver$cl == "concorde") {
    # NOTE: since concorde_path needs to set the directory and not the full path
    # to the executable, we need to apply dirname here
    TSP::concorde_path(dirname(solver$bin))
  }
  res = suppressAll(TSP::solve_TSP(instance2, method = solver$cl))
  return(
    list(
      "tour" = as.integer(res),
      "tour.length" = attr(res, "tour_length"), # TSP::tour_length(res) # hae? not working
      error = NULL,
      solver.output = NULL
    )
  )
}

#' @title
#'   Adapter/interface to the TSP algorithms implemented by the group of Markus Wagner.
#'
#' @param instance [\code{character(1)}]\cr
#'   Path to instance in TSPlib format.
#' @param control [\code{list}]\cr
#'   Control object.
#' @param solver [\code{character(1)}]\cr
#'   Solver name, i.e., on of 2APP, 2OPT or CHRIS.
#' @param bin [\code{character(1)}]\cr
#'   Full path to the binary executable.
#' @return [\code{list}]
callAustralianSolverInterface = function(instance, control, solver, bin) {
  # since this fucking Christofides implementation does not handle non-integer
  # EUC coordinates correctly we do this "transformation" here: load EUC_2D
  # instance with netgen, transform to TSP (package) instance and export again.
  requirePackages("TSP", why = paste0(solver, " TSP algorithm"))
  y = as.TSP(instance$distance.matrix)

  # set up temporary folders and files
  wd = tempdir()
  temp.file = tempfile(tmpdir = wd)

  file.input = paste0(temp.file, ".tsp")
  file.output = paste0(temp.file, ".res")

  #FIXME: hardcoded precision
  TSP::write_TSPLIB(y, file = file.input, precision = 2L)
  cur.wd = getwd()
  on.exit(setwd(cur.wd))

  # apply algorithm
  args = c(toupper(solver), file.input)
  res = try(suppressWarnings(system2(bin, args, stdout = TRUE, stderr = TRUE)))

  tour = NA
  tour.length = NA
  error = NULL

  if (inherits(res, "try-error")) {
    error = res
  }

  if (file.exists(file.output)) {
    tour = scan(file.output, what = integer(0), quiet = TRUE)
    tour = tour[-1] + 1L # since the first integer is the dimension and node numbering starts at 0
    tour.length = computeTourLength(instance, tour)
    unlink(file.output)
  }

  unlink(c(file.input, file.output))

  return(list(
      "tour" = tour,
      "tour.length" = tour.length,
      "error" = error,
      solver.output = res
    )
  )
}
