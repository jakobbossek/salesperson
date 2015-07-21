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
  if (is.null(x.path) && is.null(x)) {
    stopf("Either the instance or a path to a TSPlib file must be provided.")
  }
  if (!is.null(x)) {
    assertClass(x, "Network")
  }

  assertChoice(solver, choices = getAvailableSolverNames())

  # start time measuring
  start.time = proc.time()

  # Merge control arguments
  control = c(control, list(...))

  # get path to solver and make a first check. We need further checks in the
  # corrsponding solver functions. This is irrelevant if we use a "native" solver.
  solver.bin = solverPaths()[[solver]]

  # exporting instance for certain solvers
  if (is.null(x.path) && (solver %in% c("eax", "eax-restart", "lkh", "lkh-restart", "concorde"))) {
    # NOTE: we only have the binaries of these solvers and thus need to export
    # the instance to TSPlib format
    x.path = tempfile("TSPlib_", fileext = ".tsp")
    #x.path = "TTT.tsp"
    exportToTSPlibFormat(x, use.extended.format = FALSE, filename = x.path, name = "mist", digits = 2L)

    if (is.null(solver.bin)) {
      stopf("No path specified to executable of solver '%s'. Use the solverPaths(...) function to set a path.")
    }
  }

  if (solver %in% c("eax", "eax-restart")) {
    res = runEAXSolver(x.path, control, solver.bin, restart = (solver == "eax-restart"))
  } else if (solver %in% c("lkh", "lkh-restart")) {
    res = runLKHSolver(x.path, control, solver.bin, restart = (solver == "lkh-restart"))
  } else if (solver == "concorde") {
    res = runConcordeSolver(x.path, control, solver.bin)
  }

  # actual time measuring
  runtime = (proc.time() - start.time)[3]

  # wrap it up in a nice result object
  makeTSPSolverResult(
    instance.name = if (testClass(x, "Network")) x$name else x.path,
    solver = solver,
    tour.length = coalesce(res$tour.length, NA),
    tour = coalesce(res$tour, NA),
    runtime = runtime,
    solver.output = res$solver.output
  )
}

# Run exact CONCORDE solver.
#
# @interface see runTSPSolver
runConcordeSolver = function(instance, control, bin) {
  x = importFromTSPlibFormat(instance)

  # setup some temporary files and a temporary directory
  # Note: we need to do this here since we call a command line program
  work_dir = tempdir()
  cur_dir = getwd()
  on.exit(setwd(cur_dir))
  setwd(work_dir)

  temp_file = tempfile(tmpdir = work_dir)
  tour_file = paste0(temp_file, ".sol")
  result_file = paste0(temp_file, ".res")
  input_file = instance
  catf(input_file)

  seed = coalesce(control$seed, 1L)

  # set arguments
  args = c(
    # output file
    "-o", tour_file,
    # random seed
    "-s", seed,
    # input file
    input_file
  )

  # invoke binary. Invoke ./concorde to get a list of all possible arguments.
  res = system2(bin, args = args, stdout = TRUE, stderr = TRUE)

  # check for possible errors
  if (hasAttributes(res, "status")) {
    stopf("Error during concorde invocation.")
  }
  if (!file.access(tour_file) == 0) {
    stopf("Concorde output file could not be opened.")
  }

  # extract tour
  tour = scan(tour_file, what = integer(0), quiet = TRUE)

  # the first line contains the number of nodes. Thus we delete the first element.
  # Moreover we need to add 1 to each node, since the enumeration starts with 0.
  tour = tour[-1] + 1L

  # extract tour length
  # The first line in the result file contains the tour length as the second entry
  #FIXME: occasionally concorde outputs a *.res file from which the length could
  #be extracted. However, this is not always the case :-/
  #tour_lengh = as.integer(read.table(result_file, nrows = 1L))$V3
  tour_length = computeTourLength(x, tour, round = TRUE)

  # cleanup
  unlink(c(temp_file, result_file))

  return(list(tour = tour, tour.length = tour_length, error = NULL, solver.output = res))
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
    args$TIME_LIMIT = coalesce(control$stop.on.cutoff.time, 9999999L)
    args$OUTPUT_TOUR_FILE = paste0(instance, ".out")
    if (!is.null(control$stop.on.tour.length)) {
      args$OPTIMUM = as.integer(control$stop.on.tour.length)
      args$STOP_AT_OPTIMUM = "YES"
    }
    return(args)
  }

  writeToLKHParameterFile = function(param.file, args) {
    args = sapply(names(args), function(name) {
      sprintf("%s = %s", name, args[[name]])
    })
    output = collapse(args, "\n")
    write(output, file = param.file)
  }

  work_dir = tempdir()
  cur_dir = getwd()
  on.exit(setwd(cur_dir))
  setwd(work_dir)

  temp.file = tempfile(tmpdir = work_dir)
  param.file = paste0(temp.file, ".par")
  output.file = paste0(temp.file, ".out")
  print(output.file)
  input_file = instance
  catf(input_file)

  args = buildLKHArguments(instance, control)
  args$OUTPUT_TOUR_FILE = output.file
  writeToLKHParameterFile(param.file, args)
  # second parameter is time limit
  lkh.args = c(param.file, args$TIME_LIMIT)
  if (restart) {
    lkh.args = c("<<<", param.file)
  }

  # Write specific parameter file (deleted later)
  # $ in the output file name is replaced by tour length by LKH (see USER GUIDE)
  res = suppressWarnings(system2(lkh.bin, lkh.args, stdout = TRUE, stderr = TRUE))

  # build tour
  tour = readTSPlibTOURFile(output.file)

  # cleanup
  unlink(param.file)

  return(list("tour" = tour$tour, "tour.length" = tour$tour.length, "error" = NULL, solver.output = res))
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

  work_dir = tempdir()
  cur_dir = getwd()
  on.exit(setwd(cur_dir))
  setwd(work_dir)

  args = buildEAXArguments(instance, control, restart)
  if (!restart) {
    args$restart = NULL
  }
  args.list = unlist(args)
  res = system2(eax.bin, args.list, stdout = TRUE, stderr = TRUE)
  catf("WD: %s", getwd())
  print(list.files(getwd()))
  best.sol.conn = file(paste(basename(args$tour.file), "_BestSol", sep = ""))
  lines = readLines(best.sol.conn)

  # extract relevant data
  # first line contains #nodes and length of shortest tour found by EAX
  tour.length = as.numeric(strsplit(lines[1], " ", fixed = TRUE)[[1]][2])
  tour = as.integer(strsplit(lines[2], " ", fixed = TRUE)[[1]])

  # cleanup
  #FIXME: check the result files thorougly. What are all the numbers in the _Result file?
  #unlink(paste(args$tour.file, "_BestSol", sep = ""))
  #unlink(paste(args$tour.file, "_Result", sep = ""))

  return(list("tour" = tour, "tour.length" = tour.length, error = NULL, solver.output = res))
}
