# Run solver on instance.
#
# @param x [\code{Network}]\cr
#   TSP instance to solve.
# @param x.path [\code{character(1)}]\cr
#   File path to the TSP instance at hand in TSPlib format. Either the path or
#   the instance need to be passed.
# @param solver [\code{character(1)}]\cr
#   Name of solver to use. See \code{\link{getAvailableSolverNames}} for the
#   currently available solvers.
# @param ... [any]
#   Further parameters for the chosen solver. Not used.
# @return [\code{TSPSolverResult}]
#   Result object of type \code{TSPSolverResult}. See \code{\link{makeTSPSolverResult}}.
# @export
runTSPSolver = function(x = NULL, x.path = NULL, solver = "eax", ...) {
  # sanity checks
  if (is.null(x.path)) {
    if (is.null(x)) {
      stopf("Either the instance or a path to a TSPlib file must be provided.")
    }
    # Since our solvers need a file, we export our network here to import it
    # later again by the chosen solver
    assertClass(x, "Network")
    #x.path = tempfile("TSPlib_")
    x.path = "TSPlibFile.tsp"
    catf("Exporting to %s", x.path)
    exportToTSPlibFormat(x, use.extended.format = FALSE, filename = x.path, digits = 0L)
  }
  print(x.path)
  #assertFile(x.path, access = "r")
  assertChoice(solver, choices = getAvailableSolverNames())

  # start time measuring
  start.time = proc.time()

  # dispatching
  if (solver %in% c("eax", "eax-restart")) {
    res = runEAXSolver(x.path, solver, ...)
  } else if (solver %in% c("lkh", "lkh-restart")) {
    res = runLKHSolver(x.path, solver, ...)
  }

  # actual time measuring
  end.time = proc.time()
  runtime = (end.time - start.time)[3]

  # wrap it up in a nice result object
  makeTSPSolverResult(
    instance = if (testClass(x, "Network")) x$name else x.path,
    solver = solver,
    tour.length = coalesce(res$tour.length, NA),
    tour = coalesce(res$tour, NA),
    runtime = runtime
  )
}

# Run LKH specific stuff.
#
# @interface see runTSPSolver
runLKHSolver = function(instance, solver, ...) {
  #FIXME: LKH-1.3 support really neccessary?
  if (solver == "lkh") {
    lkh.bin = "/Users/jboss/repositories/git/salesperson/bin/lkh-2.0.7/osx/lkh"
  } else {
    lkh.bin = "/Users/jboss/repositories/git/salesperson/bin/lkh-2.0.7-restart/osx/lkh"
  }
  param.file = paste(instance, ".par", sep="")
  lkh.args = c(param.file, 9999999)

  # Write specific parameter file (deleted later)
  # $ in the output file name is replaced by tour length by LKH (see USER GUIDE)
  output.file = paste(instance, ".out", sep = "")
  write(c(paste("PROBLEM_FILE =", instance), paste("OUTPUT_TOUR_FILE =", output.file), "RUNS = 1", "SEED = 1", "MAX_TRIALS = 100000000"), file = param.file)
  res = suppressWarnings(system2(lkh.bin, lkh.args, stdout = TRUE))

  # build tour
  tour = as.integer(readTSPlibTOURFile(output.file))

  x = paste(res)

  # cleanup
  unlink(param.file)

  return(list("tour" = tour, "error" = NULL))
}

# Run EAX specific stuff.
#
# @interface see runTSPSolver
runEAXSolver = function(instance, solver, ...) {
  if (solver == "eax") {
    eax.bin = "/Users/jboss/repositories/git/salesperson/bin/eax/osx/eax"
  } else {
    eax.bin = "/Users/jboss/repositories/git/salesperson/bin/eax-restart/osx/eax"
  }
  #FIXME: does not work as expected. Generate tempfile in tempdir!
  #temp.file = tempfile("EAX_")
  temp.file = paste(instance, ".out", sep = "")
  #FIXME: meaning of all these parameters?
  eax.args = c(1, temp.file, 100, 30, instance, 0, 3)
  if (solver == "eax-restart") {
    eax.args = c(eax.args, 1)
  }
  res = system2(eax.bin, eax.args, stdout = TRUE)
  best.sol.conn = file(paste(temp.file, "_BestSol", sep = ""))
  lines = readLines(best.sol.conn)

  # extract relevant data
  # first line contains #nodes and length of shortest tour found by EAX
  tour.length = as.numeric(strsplit(lines[1], " ", fixed = TRUE)[[1]][2])
  tour = as.integer(strsplit(lines[2], " ", fixed = TRUE)[[1]])

  # cleanup
  #FIXME: check the result files thorougly. What are all the numbers in the _Result file?
  unlink(paste(temp.file, "_BestSol", sep = ""))
  unlink(paste(temp.file, "_Result", sep = ""))

  return(list("tour" = tour, "tour.length" = tour.length, error = NULL))
}
