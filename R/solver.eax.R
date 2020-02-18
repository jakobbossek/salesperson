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

# INTERNAL
# Helper function to write file with initial solutions for EAX
# algorithm.
#
# @param init.pop [list]
#   Initial population. See docs of run.eax for more information.
# @param file.init.pop [character(1)]
#   Path to file in which the initial population should be stored.
# @return Nothing
writeInitialPopulation = function(init.pop, file.init.pop) {
  con = file(file.init.pop, "w")
  on.exit(close(con))
  for (i in 1:length(init.pop)) {
    line1 = sprintf("%i %i",as.integer(init.pop[[i]]$n), as.integer(init.pop[[i]]$tour.length))
    writeLines(line1, con = con)
    line2 = collapse(init.pop[[i]]$tour, sep = " ")
    writeLines(line2, con = con)
  }
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
#' @param cutoff.time [\code{numeric(1)}]\cr
#'   Maximal running time in seconds.
#'   Default is 10.
#' @template arg_opt_tour_length
#' @template arg_seed
#' @param with.restarts [\code{logical(1)}]\cr
#'   Should EAX restart if a plateau is reached?
#'   Default is \code{FALSE}.
#' @param with.GPX [\code{logical(1)}]\cr
#'   Activate GPX2 crossover?
#'   Default is \code{FALSE}.
#' @param snapshot.step [\code{integer(1)}]\cr
#'   Possibility to log the entire population each \code{snapshot.step}
#'   times.
#'   Default is \code{0}, i.e., do not log at all.
#' @template arg_full_matrix
#' @template arg_verbose
#' @template arg_log_trajectory
#' @template arg_work_dir
#' @template arg_output_files_prefix
#' @template arg_keep_output_files
#' @param init.pop [\code{list}]\cr
#'   List of lists. Each sublist needs to contains three components:
#'   \describe{
#'     \item{n [\code{integer(1)}]}{Number of nodes of the TSP problem.}
#'     \item{tour [\code{integer(n)}]}{The actual tour.}
#'     \item{tour.length [\code{integer(1)}]}{Length of the tour.}
#'   }
#'   Default is \code{NULL}, i.e., the initial population is generated
#'   randomly and 2-Opt is applied to each solution before the evolutionary
#'   loop starts.
#' @param ... [any]\cr
#'   Not used at the moment.
#' @template ret_TSPSolverResult
#' @export
run.eax = function(solver, instance,
  max.trials = 1L,
  pop.size = 100L,
  off.size = 30L,
  cutoff.time = 10,
  opt.tour.length = NULL,
  seed = as.integer(ceiling(runif(1L) * 2^15)),
  with.restarts = FALSE,
  with.GPX = FALSE,
  snapshot.step = 0L,
  full.matrix = FALSE,
  verbose = FALSE,
  log.trajectory = TRUE,
  work.dir = NULL,
  output.files.prefix = NULL,
  keep.output.files = FALSE,
  init.pop = NULL,
  ...) {
  # sanity check stuff
  max.trials = asInt(max.trials, lower = 1L)
  pop.size = asInt(pop.size, lower = 2L)
  off.size = asInt(off.size, lower = 1L)

  # passing 0 to binary deactivates cutoff time
  if (is.null(cutoff.time))
    cutoff.time = 0

  cutoff.time = as.numeric(cutoff.time)

  # passing 0 to binary means: optimum is not known
  if (is.null(opt.tour.length))
    opt.tour.length = 0L
  opt.tour.length = asInt(opt.tour.length, lower = 0L)

  seed = asInt(seed, lower = 1L)
  assertFlag(with.restarts)
  assertFlag(with.GPX)

  # 0 deactivates snapshots
  snapshot.step = asInt(snapshot.step, lower = 0L)

  assertString(work.dir, null.ok = TRUE)
  assertString(output.files.prefix, null.ok = TRUE)
  assertFlag(keep.output.files)
  assertFlag(full.matrix)
  assertFlag(verbose)
  assertFlag(log.trajectory)

  # temporary work dir
  work.dir = BBmisc::coalesce(work.dir, tempdir())
  temp.file = BBmisc::coalesce(output.files.prefix, basename(tempfile(tmpdir = work.dir)))

  # handle directory change
  cur.wd = getwd()
  setwd(work.dir)
  on.exit(setwd(cur.wd))

  file.init.pop = NULL
  if (!is.null(init.pop)) {
    assertList(init.pop, max.len = pop.size, any.missing = FALSE, all.missing = FALSE)
    file.init.pop = paste0(temp.file, "_init.pop")
    writeInitialPopulation(init.pop, file.init.pop)
  }

  # in case we pass a Network object, check whether its compatible with
  # EAX and export accordingly
  is.temp.input = FALSE
  if (testClass(instance, "Network")) {
    file.input = paste0(temp.file, ".tsp")
    is.temp.input = TRUE
    if (full.matrix && any(round(instance$distance.matrix) != instance$distance.matrix)) {
      stopf("EAX can handle only integer distances!")
    }
    exportToTSPlibFormat(instance, filename = file.input, full.matrix = full.matrix, use.extended.format = FALSE)
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
  # Generic example call: ./main trials DATA fNumOfPop fNumOfCh instance tourLength cutoff seed doRestart snapshotStep logTrajectory withGPX initPopFileName
  # Example call: ./main 1 DATA 100 30 instances/rat575.tsp 0 20 1 0 0 1 0
  args = list(max.trials, file.output, pop.size, off.size,
    file.input, opt.tour.length, cutoff.time, seed, as.integer(with.restarts),
    snapshot.step, as.integer(log.trajectory), as.integer(with.GPX))

  if (!is.null(init.pop))
    args = c(args, file.init.pop)

  # try to call solver
  start.time = proc.time()
  solver.output = system2(solver$bin, args, stdout = verbose, stderr = verbose)
  runtime = as.numeric(proc.time() - start.time)

  if (verbose)
    print(solver.output)
  tour = readEAXSolution(file.sol)

  trajectory = NULL
  if (log.trajectory)
    trajectory = read.table(file.trajectory, header = TRUE, sep = ",", stringsAsFactors = FALSE)

  # cleanup
  if (is.temp.input)
    unlink(file.input)

  if (!keep.output.files)
    unlink(c(file.output, file.sol, file.result, file.trajectory))

  if (!is.null(init.pop))
    unlink(file.init.pop)

  solver.id = sprintf("EAX%s%s",
    if(with.restarts) "+restart" else "",
    if (with.GPX) "+GPX" else "")

  list(
    solver.id = solver.id,
    tour = tour$tour,
    tour.length = tour$tour.length,
    trajectory = trajectory,
    runtime = runtime,
    error = NULL,
    solver.output = solver.output
  )
}
