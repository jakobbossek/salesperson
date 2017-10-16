#' @export
makeTSPSolver.eax = function() {
  makeTSPSolverInternal(
    cl = "eax",
    short.name = "EAX",
    name = "Edge-Assembly-Crossover",
    properties = c("euclidean", "external", "requires.tsplib")
  )
}

readEAXSolution = function(file.sol) {
  sol.con = file(file.sol, "r")
  lines = readLines(sol.con)
  close(sol.con)

  # extract relevant data
  # first line contains #nodes and length of shortest tour found by EAX
  list(
    tour.length = as.numeric(strsplit(lines[1], " ", fixed = TRUE)[[1]][2]),
    tour = as.integer(strsplit(lines[2], " ", fixed = TRUE)[[1]])
  )
}

#' @export
# @interface see runTSPSolver
#FIXME: add possibility to return trajectory file instead of imported trajectory
#FIXME: handle initial population
run.eax = function(solver, instance,
  max.trials = 1L,
  pop.size = 100L,
  off.size = 30L,
  cutoff.time = 10L,
  opt.tour.length = NULL,
  seed = as.integer(runif(1L) * 2^15),
  with.restarts = FALSE,
  snapshot.step = 0L,
  full.matrix = FALSE,
  verbose = FALSE,
  return.trajectory.file = FALSE,
  ...) {
  # sanity check stuff
  max.trials = asInt(max.trials, lower = 1L)
  pop.size = asInt(pop.size, lower = 2L)
  off.size = asInt(off.size, lower = 1L)

  # passing 0 to binary deactivates cutoff time
  if (is.null(cutoff.time))
    cutoff.time = 0L
  else
    cutoff.time = asInt(cutoff.time, lower = 0L)

  # passing 0 to binary means: optimum is not known
  if (is.null(opt.tour.length))
    opt.tour.length = 0L
  else
    opt.tour.length = asInt(opt.tour.length, lower = 1L)

  seed = asInt(seed, lower = 1L)
  assertFlag(with.restarts)

  # 0 deactivates snapshots
  snapshot.step = asInt(snapshot.step, lower = 0L)

  assertFlag(full.matrix)
  assertFlag(verbose)
  assertFlag(return.trajectory.file)

  # temporary work dir
  temp.dir = tempdir()
  temp.file = basename(tempfile(tmpdir = temp.dir))

  # handle directory change
  cur.wd = getwd()
  setwd(temp.dir)
  on.exit(setwd(cur.wd))

  # in case we pass a Network object, check whether its compatible with
  # EAX and export accordingly
  is.temp.input = FALSE
  if (testClass(instance, "Network")) {
    file.input = paste0(temp.file, ".tsp")
    is.temp.input = TRUE
    if (full.matrix && any(round(instance$distance.matrix) != instance$distance.matrix)) {
      stopf("EAX can handle only integer distances!")
    }
    netgen::exportToTSPlibFormat(instance, filename = file.input, full.matrix = full.matrix, use.extended.format = FALSE)
  } else {
     file.input = instance
  }
  assertFile(file.input, "r")

  # build filenames for file which store the results
  file.output = paste0(temp.file, ".out")
  file.sol = paste0(temp.file, ".out_BestSol")
  file.result = paste0(temp.file, ".out_Result")
  file.trajectory = paste0(temp.file, ".out_Incumbant")

  # See solvers/eax/README.md for details
  # Examplary call to EAX: #./jikken trials DATA pop off rat575.tsp opt cutoff seed withRestarts snapshot
  args = list(max.trials, file.output, pop.size, off.size,
    file.input, opt.tour.length, cutoff.time, seed, as.integer(with.restarts),
    snapshot.step)

  # try to call solver
  solver.output = system2(solver$bin, args, stdout = verbose, stderr = verbose)
  tour = readEAXSolution(file.sol)

  trajectory = if (!return.trajectory.file)
    read.table(file.trajectory, header = TRUE, sep = ",")
  else
    file.path(temp.dir, file.trajectory)

  # cleanup
  unlink(c(file.output, file.sol, file.result))
  if (is.temp.input) {
    unlink(file.input)
  }

  if (!return.trajectory.file)
    unlink(file.trajectory)

  list(
    tour = tour$tour,
    tour.length = tour$tour.length,
    trajectory = trajectory,
    error = NULL,
    solver.output = solver.output
  )
}
