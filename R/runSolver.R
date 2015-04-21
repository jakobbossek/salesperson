# Run solver on instance.
#
# @param instance [\code{character(1)}]\cr
#   File path to TSPlib file.
# @param solver [\code{character(1)}]\cr
#   Name of solver to use. See \code{\link{getAvailableSolverNames}} for the
#   currently available solvers.
# @param ... [any]
#   Further parameters for the chosen solver. Not used.
# @return [\code{TSPSolverResult}]
#   Result object of type \code{TSPSolverResult}. See \code{\link{makeTSPSolverResult}}.
# @export
#FIXME: add the possibility to pass netgen Networks
runTSPSolver = function(instance, solver, ...) {
  # sanity checks
  assertCharacter(instance, len = 1L, any.missing = FALSE)
  assertChoice(solver, choices = getAvailableSolverNames())

  # start time measuring
  start.time = proc.time()

  # dispatching
  if (solver %in% c("eax", "eax-restart")) {
    res = runEAXSolver(instance, solver, ...)
  } else if (solver %in% c("lkh", "lkh-restart")) {
    res = runLKHSolver(instance, solver, ...)
  }

  # actual time measuring
  end.time = proc.time()
  runtime = (end.time - start.time)[3]

  # wrap it up in a nice result object
  makeTSPSolverResult(
    instance.name = instance,
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
  #FIXME: we need to make this more general. Since shipping binaries is not
  # allowed in R packages, the user needs to download the binary by hand and
  # set the correct path. Maybe do this via R options? This way it would
  # be sufficient to set options only once and not every time we use the solver.
  #FIXME: LKH-1.3 support really neccessary?
  #FIXME: parse LKH output. We need the tour length!
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
  #FIXME: add possibility to pass LKH arguments
  write(c(paste("PROBLEM_FILE =", instance), paste("OUTPUT_TOUR_FILE =", output.file), "RUNS = 1", "SEED = 1", "MAX_TRIALS = 100000000"), file = param.file)
  res = suppressWarnings(system2(lkh.bin, lkh.args, stdout = TRUE))

  # build tour
  tour = as.integer(readTSPlibTOURFile(output.file))

  #FIXME: parse results, extract relevent information
  #FIXME: how to check for error?
  x = paste(res)

  # cleanup
  unlink(param.file)

  return(list("tour" = tour, "error" = NULL))
}

# Run EAX specific stuff.
#
# @interface see runTSPSolver
runEAXSolver = function(instance, solver, ...) {
  #FIXME: we need to make this more global, i.e., maybe 'TSP::concorde_path' like
  if (solver == "eax") {
    eax.bin = "/Users/jboss/repositories/git/salesperson/bin/eax/osx/eax"
  } else {
    eax.bin = "/Users/jboss/repositories/git/salesperson/bin/eax-restart/osx/eax"
  }
  #FIXME: does not work as expected. Generate tempfile in tempdir!
  #temp.file = tempfile("EAX_")
  temp.file = paste(instance, ".out", sep = "")
  #FIXME: add possibility to pass arguments to EAX
  #FIXME: meaning of all these parameters?
  eax.args = c(1, temp.file, 100, 30, instance, 0, 3)
  if (solver == "eax-restart") {
    eax.args = c(eax.args, 1)
  }
  res = suppressWarnings(system2(eax.bin, eax.args, stdout = TRUE))
  #FIXME: how to check whether algo was successful?
  best.sol.conn = file(paste(temp.file, "_BestSol", sep = ""))
  #FIXME: maybe there is no solution file! Catch error and handle
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
