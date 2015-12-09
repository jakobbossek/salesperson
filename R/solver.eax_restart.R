#' @export
makeTSPSolver.eax_restart = function() {
  makeTSPSolverInternal(
    cl = "eax_restart",
    short.name = "EAX-restart",
    name = "Edge-Assembly-Crossover with restart on stagnation",
    properties = c("euclidean", "external", "requires.tsplib"),
    #FIXME: here we misuse learner parameters as solver parameter, but i think it is ok
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "max.trials", default = 1L, lower = 1L),
      makeIntegerLearnerParam(id = "pop.size", default = 100L, lower = 2L),
      makeIntegerLearnerParam(id = "off.size", default = 30L),
      makeIntegerLearnerParam(id = "cutoff.time", default = 999999999L),
      makeNumericLearnerParam(id = "opt.tour.length", default = 0, lower = 0),
      #makeIntegerLearnerParam(id = "max.iter.with.no.improvement", default = 10, lower = 1L),
      # zero means no restarts at all
      #makeIntegerLearnerParam(id = "max.restarts", default = 10L),
      #makeLogicalLearnerParam(id = "stop.on.best.close.to.average", default = TRUE),
      makeIntegerLearnerParam(id = "seed", default = 1L),
      # the following paraemters a not parameters of the C++ implementation
      makeLogicalLearnerParam(id = "full.matrix", default = FALSE)
    )
  )
}

#' @export
# @interface see runTSPSolver
run.eax_restart = function(solver, instance, solver.pars, ...) {
  temp.dir = tempdir()
  temp.file = basename(tempfile(tmpdir = temp.dir))

  cur.wd = getwd()
  setwd(temp.dir)
  on.exit(setwd(cur.wd))

  is.temp.input = FALSE
  if (testClass(instance, "Network")) {
    full.matrix = coalesce(solver.pars$full.matrix, FALSE)
    file.input = paste0(temp.file, ".tsp")
    is.temp.input = TRUE
    if (full.matrix && any(round(instance$distance.matrix) != instance$distance.matrix)) {
      stopf("EAX+restart can handle only integer distances!")
    }
    netgen::exportToTSPlibFormat(instance, filename = file.input, full.matrix = full.matrix, use.extended.format = FALSE)
  } else {
    file.input = instance
  }
  assertFile(file.input, "r")

  # build filenames for file which store the results
  file.output = paste0(file.input, ".out")
  file.sol = paste0(file.input, ".out_BestSol")
  file.result = paste0(file.input, ".out_Result")

  # See solvers/eax/README.md for details
  # Examplary call to EAX+restart: ./jikken 10 DATA 100 30 rat575.tsp 0 60 123
  args = list()
  args$max.trials = coalesce(solver.pars$max.trials, 1L)
  args$tour.file = file.output
  args$pop.size = coalesce(solver.pars$pop.size, 100L)
  args$off.size = coalesce(solver.pars$off.size, 30L)
  args$instance.file = file.input
  args$opt.tour.length = coalesce(solver.pars$opt.tour.length, 0L)
  args$cutoff.time = coalesce(solver.pars$cutoff.time, 999999L)
  # args$max.iter.with.no.improvement = coalesce(solver.pars$max.iter.with.no.improvement, 999999L)
  # args$max.restarts = coalesce(solver.pars$max.restarts, 10L)
  # args$stop.on.best.close.to.average = coalesce(solver.pars$stop.on.best.close.to.average, 1L)
  args$seed = coalesce(solver.pars$seed, 123L)

  args.list = unlist(args)

  # IMPORTANT: append runsolver stuff
  # This is ugly as sin, but I do not know a better/nicer solution at the moment
  #args.list = c("-C", solver.pars$stop.on.cutoff.time, "-w", runsolver.file, eax.bin, args.list)

  # try to call solver
  res = try(suppressWarnings(system2(solver$bin, args.list, stdout = TRUE, stderr = TRUE)))

  # prepare result
  tour = NA
  tour.length = NA
  error = NULL
  solver.output = NULL
  #runsolver.output = NULL

  # runsolver.con = file(runsolver.file, "r")
  # runsolver.output = readLines(runsolver.con)
  # close(runsolver.con)

  #sol.file = paste0(args$tour.file, "_BestSol")

  # algorithm probably failed: try to extract the best solution so far
  # if (!file.exists(sol.file)) {
  #   catf("Most probably %s did not finish before time limit was reached.", solver$bin)
  #   error = "Most probably the algorithm reached the time limit."
  #   # run failed forced by runsolver or unknown error occured. Extract last known tour length from stdout
  #   if (!is.null(res) && length(res) > 0L) {
  #     last.output = tail(res, n = 10L)
  #     for (i in length(last.output):1) {
  #       output.line = last.output[i]
  #       # extract lines with three numbers
  #       bags = str_extract_all(output.line, "[0-9.]+")[[1]]
  #       # if we find the first line of type "integer: integer float", then we found the last output
  #       # of the best tour length
  #       if (length(bags) == 3L) {
  #         # select best tour so far as the result
  #         tour.length = as.integer(bags[2])
  #         break
  #       }
  #     }
  #   }
  # } else {
  sol.con = file(file.sol, "r")
  lines = readLines(sol.con)
  close(sol.con)

  # extract relevant data
  # first line contains #nodes and length of shortest tour found by EAX
  #FIXME: we can make this nicer!
  tour.length = as.numeric(strsplit(lines[1], " ", fixed = TRUE)[[1]][2])
  tour = as.integer(strsplit(lines[2], " ", fixed = TRUE)[[1]])

  # cleanup
  unlink(c(file.output, file.sol, file.result))
  if (is.temp.input) {
    unlink(file.input)
  }

  return(
    list(
      "tour" = tour,
      "tour.length" = tour.length,
      error = error,
      solver.output = res
      #runsolver.output = runsolver.output
    )
  )
}
