#' @export
makeTSPSolver.eax = function() {
  makeTSPSolverInternal(
    cl = "eax",
    short.name = "EAX",
    name = "Edge-Assembly-Crossover",
    properties = c("euclidean", "external", "requires.tsplib")
  )
}

# INTERNAL
# Helper function to extract tour and tour length of a solution
# file produced by EAX binary.
#
# @param file.sol [character(1)]
#   Path to solution file.
# @return [list] With components tour.length and tour.
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

#' @title Solver: EAX
#'
#' @description Inexact TSP solvers based on a genetic approach.
#'
#' @note This solver requires integer inter-city distances.
#'
#' @references
#' Nagata, Y. and Kobayashi, S. (2013). A powerful genetic algorithm using
#' edge assembly crossover for the travelling salesman problem. INFORMS Journal
#' on Computing, 25(2):346-363.
#'
#' Nagata, Y. and Kobayashi, S. (1997). Edge assembly crossover: A high-power
#' genetic algorithm for the travelling salesman problem. In Baeck, T., editor,
#' Proceedings of the Seventh International Conference on Genetic Algorithms
#' (ICGA97), pages 450-457, San Francisco, CA. Morgan Kaufmann.
#'
#' @template arg_solver
#' @template arg_instance
#' @param max.trials [\code{integer(1)}]\cr
#'   Number of independent runs. At the moment this is fixed to 1.
#' @param pop.size [\code{integer(1)}]\cr
#'   Population size.
#'   Default is 100.
#' @param off.size [\code{integer(1)}]\cr
#'   Number of offspring generated in each generation.
#'   Default is 30.
#' @param cutoff.time [\code{integer(1)}]\cr
#'   Maximal running time in seconds.
#'   Default is 10.
#' @template arg_opt_tour_length
#' @template arg_seed
#' @param with.restarts [\code{logical(1)}]\cr
#'   Should EAX restart if a plateau is reached?
#'   Default is \code{FALSE}.
#' @param snapshot.step [\code{integer(1)}]\cr
#'   Possibility to log the entire population each \code{snapshot.step}
#'   times.
#'   Default is \code{0}, i.e., do not log at all.
#' @template arg_full_matrix
#' @template arg_verbose
#' @param return.trajectory.file [\code{logical(1)}]\cr
#'   If set to \code{FALSE} (the default), the logged optimization trace / trajectory
#'   is returned as a data frame. However, long solver runs may produce a
#'   trajectory of substantial size. If this is expected the user may decide
#'   to return just the path to the csv file the trajectory is stored to instead
#'   of importing this file.
#' @param ... [any]\cr
#'   Not used at the moment.
#' @template ret_TSPSolverResult
#' @export
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
