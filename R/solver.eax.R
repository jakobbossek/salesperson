#' @export
makeTSPSolver.eax = function() {
  makeTSPSolverInternal(
    cl = "eax",
    short.name = "EAX",
    name = "Edge-Assembly-Crossover",
    properties = c("euclidean", "external", "requires.tsplib"),
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "max.trials", default = 1L, lower = 1L),
      makeIntegerLearnerParam(id = "pop.size", default = 100L, lower = 2L),
      makeIntegerLearnerParam(id = "off.size", default = 30L),
      makeIntegerLearnerParam(id = "cutoff.time", default = 999999999L),
      makeNumericLearnerParam(id = "opt.tour.length", default = 0, lower = 0),
      makeIntegerLearnerParam(id = "seed", default = 1L),
      makeLogicalLearnerParam(id = "with.restarts", default = FALSE),
      # the following paraemters a not parameters of the C++ implementation
      makeLogicalLearnerParam(id = "full.matrix", default = FALSE)
    )
  )
}

#' @export
# @interface see runTSPSolver
run.eax = function(solver, instance, solver.pars, ...) {
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
    full.matrix = coalesce(solver.pars$full.matrix, FALSE)
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
  # Examplary call to EAX: #./jikken 10 DATA 100 30 rat575.tsp 0 60 seed
  args = list()
  args$max.trials = coalesce(solver.pars$max.trials, 1L)
  args$tour.file = file.output
  args$pop.size = coalesce(solver.pars$pop.size, 100L)
  args$off.size = coalesce(solver.pars$off.size, 30L)
  args$instance.file = file.input
  args$opt.tour.length = coalesce(solver.pars$opt.tour.length, 0L)
  args$cutoff.time = coalesce(solver.pars$cutoff.time, 999999L)

  args$seed = coalesce(solver.pars$seed, 123L)
  args$with.restarts = coalesce(solver.pars$with.restarts, 0)

  args.list = unlist(args)

  # try to call solver
  solver.output = system2(solver$bin, args.list, stdout = TRUE, stderr = TRUE)

  # prepare result
  tour = NA
  tour.length = NA
  error = NULL

  sol.con = file(file.sol, "r")
  lines = readLines(sol.con)
  close(sol.con)

  # extract relevant data
  # first line contains #nodes and length of shortest tour found by EAX
  #FIXME: we can make this nicer!
  tour.length = as.numeric(strsplit(lines[1], " ", fixed = TRUE)[[1]][2])
  tour = as.integer(strsplit(lines[2], " ", fixed = TRUE)[[1]])

  trajectory = read.table(file.trajectory, header = TRUE, sep = ",")

  # cleanup
  unlink(c(file.output, file.sol, file.result, file.trajectory))
  if (is.temp.input) {
    unlink(file.input)
  }

  return(
    list(
      "tour" = tour,
      "tour.length" = tour.length,
      "trajectory" = trajectory,
      error = error,
      solver.output = solver.output
    )
  )
}
