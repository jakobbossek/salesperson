#' @export
makeTSPSolver.lkh = function() {
  makeTSPSolverInternal(
    cl = "lkh",
    short.name = "LKH",
    name = "Lin-Kernigham Heuristic",
    properties = c("euclidean", "external", "requires.tsplib")
  )
}

writeToLKHParameterFile = function(file.params, args) {
  args = sapply(names(args), function(name) {
    if (is.integer(args[[name]])) {
      sprintf("%s = %i", name, args[[name]])
    } else if (is.numeric(args[[name]])) {
      sprintf("%s = %f", name, args[[name]])
    } else {
      sprintf("%s = %s", name, args[[name]])
    }
  })
  output = collapse(args, "\n")
  write(output, file = file.params)
}

#' @export
# @interface see runTSPSolver
run.lkh = function(solver, instance,
  runs = 1L,
  seed = as.integer(runif(1L) * 2^15),
  cutoff.time = 0,
  opt.tour.length = NULL,
  max.trials = 1000L,
  full.matrix = FALSE,
  verbose = FALSE,
   ...) {

  runs = asInt(runs, lower = 1L)
  seed = asInt(seed, lower = 1L)
  if (!is.null(cutoff.time))
    cutoff.time = asInt(cutoff.time, lower = 1L)

  if (!is.null(opt.tour.length))
    opt.tour.length = asInt(opt.tour.length, lower = 1L)

  max.trials = asInt(max.trials, lower = 10L)

  assertFlag(full.matrix)
  assertFlag(verbose)

  temp.dir = tempdir()
  cur.dir = getwd()
  on.exit(setwd(cur.dir))
  setwd(temp.dir)

  temp.file = basename(tempfile(tmpdir = temp.dir))
  file.params = paste0(temp.file, ".par")
  file.output = paste0(temp.file, ".out")
  file.trajectory = paste0(temp.file, ".traj")

  has.temporary.input = FALSE
  if (testClass(instance, "Network")) {
    file.input = paste0(temp.file, ".tsp")
    has.temporary.input = TRUE
    if (full.matrix && any(round(instance$distance.matrix) != instance$distance.matrix)) {
      stopf("LKH can handle only integer distances!")
    }
    netgen::exportToTSPlibFormat(instance, filename = file.input, full.matrix = full.matrix, use.extended.format = FALSE, digits = 100)
  } else {
    file.input = instance
  }

  # the most important parameters are the PROBLEM_FILE, the number of RUNS,
  # the initial SEED for the generator of pseudo random numbers and TIME_LIMIT
  # in seconds.
  args = list()
  args$PROBLEM_FILE = file.input
  args$OUTPUT_TRAJECTORY_FILE = file.trajectory
  args$RUNS = runs
  args$SEED = seed
  if (!is.null(cutoff.time))
    args$TIME_LIMIT = cutoff.time

  args$MAX_TRIALS = max.trials

  if (!is.null(opt.tour.length)) {
    args$STOP_AT_OPTIMUM = "YES"
    args$OPTIMUM = opt.tour.length
  }

  args$OUTPUT_TOUR_FILE = file.output
  writeToLKHParameterFile(file.params, args)

  # Write specific parameter file (deleted later)
  # $ in the output file name is replaced by tour length by LKH (see USER GUIDE)
  res = system2(solver$bin, args = file.params, stdout = verbose, stderr = verbose)

  # build tour
  tmp = readTSPlibTOURFile(file.output)
  tour = tmp$tour
  tour.length = tmp$tour.length
  trajectory = read.table(file.trajectory, header = TRUE, sep = ",")

  unlink(c(file.output, file.params, file.trajectory))
  if (has.temporary.input) {
    unlink(file.input)
  }

  return(list(
      "tour" = tour,
      "tour.length" = tour.length,
      "error" = NULL,
      "trajectory" = trajectory,
      solver.output = res
    )
  )
}
