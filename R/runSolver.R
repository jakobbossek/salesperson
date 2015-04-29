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
# @param control [\code{list}]\cr
#   Named list of further arguments passed down the the solver.
# @param ... [any]
#   Further parameters for the chosen solver. Merged with \code{control} and
#   passed to the server.
# @return [\code{TSPSolverResult}]
#   Result object of type \code{TSPSolverResult}. See \code{\link{makeTSPSolverResult}}.
# @export
runTSPSolver = function(x = NULL, x.path = NULL, solver = "eax", control = list(), ...) {
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
    messagef("Exporting to %s", x.path)
    exportToTSPlibFormat(x, use.extended.format = FALSE, filename = x.path, digits = 0L)
  }
  #assertFile(x.path, access = "r")
  assertChoice(solver, choices = getAvailableSolverNames())

  # start time measuring
  start.time = proc.time()

  # Merge control arguments
  control = c(control, list(...))

  # dispatching
  if (solver %in% c("eax", "eax-restart", "lkh", "lkh-restart")) {
    # get path to solver and make a first check. We need further checks in the
    # corrsponding solver functions.
    solver.bin = solverPaths()[[solver]]
    if (is.null(solver.bin)) {
      stopf("No path specified to executable of solver '%s'. Use the solverPaths function to set a path.")
    }
    if (solver %in% c("eax", "eax-restart")) {
      res = runEAXSolver(x.path, control, solver.bin, restart = (solver == "eax-restart"))
    } else if (solver %in% c("lkh", "lkh-restart")) {
      res = runLKHSolver(x.path, control, solver.bin, restart = (solver == "lkh-restart"))
    }
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
runLKHSolver = function(instance, control, lkh.bin, restart = FALSE) {
  buildLKHArguments = function(instance, control) {
    args = list()
    # the most important parameters are the PROBLEM_FILE, the number of RUNS,
    # the initial SEED for the generator of pseudo random numbers and TIME_LIMIT
    # in seconds.
    args$PROBLEM_FILE = instance
    args$RUNS = coalesce(control$runs, 1L)
    args$SEED = coalesce(control$seed, 1L)
    args$TIME_LIMIT = coalesce(control$time.limit, 9999999L)
    args$OUTPUT_TOUR_FILE = paste0(instance, ".out")
    return(args)
  }

  writeToLKHParameterFile = function(param.file, args) {
    args = sapply(names(args), function(name) {
      sprintf("%s = %s", name, args[[name]])
    })
    output = collapse(args, "\n")
    write(output, file = param.file)
  }

  param.file = paste0(instance, ".par")
  args = buildLKHArguments(instance, control)
  writeToLKHParameterFile(param.file, args)
  # second parameter is time limit
  lkh.args = c(param.file, if (!is.null(args$TIME_LIMIT)) args$TIME_LIMIT else 1000000L)
  if (restart) {
    lkh.args = c("<<<", param.file)
  }

  # Write specific parameter file (deleted later)
  # $ in the output file name is replaced by tour length by LKH (see USER GUIDE)
  res = suppressWarnings(system2(lkh.bin, lkh.args, stdout = TRUE, stderr = TRUE))
  print(res)

  # build tour
  tour = readTSPlibTOURFile(args$OUTPUT_TOUR_FILE)

  x = paste(res)

  # cleanup
  unlink(param.file)

  return(list("tour" = tour$tour, "tour.length" = tour$tour.length, "error" = NULL))
}

# Run EAX specific stuff.
#
# @interface see runTSPSolver
runEAXSolver = function(instance, control, eax.bin, restart = TRUE) {
  # 0. argv[0] name of the function (as usual in c)
  # 1. maxNumOfTrial: number of trials, i.e., restarts?
  # 2. dstFile: most probably file to store data of tour
  # 3. fNumOfPop: number of populations (mu) or iterations?
  # 4. fNumOfKids: number of offspring (lambda?)
  # 5. fFileNameTSP: file name of tsp source file (in TSPlib format)
  # 6. fTargetTourLength:
  # 7. fCutoffTime:
  buildEAXArguments = function(instance, control, restart) {
    args = list()
    args$max.trials = coalesce(control$max.trials, 1L)
    args$tour.file = coalesce(control$tour.file, paste0(instance, ".out"))
    args$pop.size = coalesce(control$pop.size, 100L)
    args$off.size = coalesce(control$off.size, 30L)
    args$instance.file = instance
    args$stop.on.tour.length = coalesce(control$stop.on.tour.length, 0)
    args$stop.on.cutoff.time = coalesce(control$stop.on.cutoff.time, 3)
    args$restart = coalesce(control$restart, 1L)
    return(args)
  }

  args = buildEAXArguments(instance, control, restart)
  if (!restart) {
    args$restart = NULL
  }
  args.list = unlist(args)
  res = system2(eax.bin, args.list, stdout = TRUE)
  best.sol.conn = file(paste(args$tour.file, "_BestSol", sep = ""))
  lines = readLines(best.sol.conn)

  # extract relevant data
  # first line contains #nodes and length of shortest tour found by EAX
  tour.length = as.numeric(strsplit(lines[1], " ", fixed = TRUE)[[1]][2])
  tour = as.integer(strsplit(lines[2], " ", fixed = TRUE)[[1]])

  # cleanup
  #FIXME: check the result files thorougly. What are all the numbers in the _Result file?
  unlink(paste(args$tour.file, "_BestSol", sep = ""))
  unlink(paste(args$tour.file, "_Result", sep = ""))

  return(list("tour" = tour, "tour.length" = tour.length, error = NULL))
}
