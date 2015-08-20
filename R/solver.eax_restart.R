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
      #FIXME: the next one is not yet implemented
      makeIntegerLearnerParam(id = "max.iter.with.no.improvement", default = 10, lower = 1L),
      # zero means no restarts at all
      makeIntegerLearnerParam(id = "max.restarts", default = 10L),
      makeLogicalLearnerParam(id = "stop.on.best.close.to.average", default = TRUE),
      makeIntegerLearnerParam(id = "seed", default = 1L)
    )
  )
}

#' @export
prepareInstance.eax_restart = function(solver, instance) {
  return(prepareInstance.eax(solver, instance))
}

#' @export
# @interface see runTSPSolver
run.eax_restart = function(solver, instance, control) {
  # See solvers/eax/README.md for details
  args = list()
  args$max.trials = coalesce(control$max.trials, 1L)
  args$tour.file = coalesce(control$tour.file, paste0(instance, ".out"))
  args$pop.size = coalesce(control$pop.size, 100L)
  args$off.size = coalesce(control$off.size, 30L)
  args$instance.file = instance
  args$opt.tour.length = coalesce(control$opt.tour.length, 0L)
  args$cutoff.time = coalesce(control$cutoff.time, 999999L)
  args$max.iter.with.no.improvement = coalesce(control$max.iter.with.no.improvement, 999999L)
  args$max.restarts = coalesce(control$max.restarts, 10L)
  args$stop.on.best.close.to.average = coalesce(control$stop.on.best.close.to.average, 1L)
  args$seed = coalesce(control$seed, 123L)

  args.list = unlist(args)

  # IMPORTANT: append runsolver stuff
  # This is ugly as sin, but I do not know a better/nicer solution at the moment
  #args.list = c("-C", control$stop.on.cutoff.time, "-w", runsolver.file, eax.bin, args.list)

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

  sol.file = paste0(args$tour.file, "_BestSol")

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
  best.sol.conn = file(sol.file, "r")
  lines = readLines(best.sol.conn)
  close(best.sol.conn)

  # extract relevant data
  # first line contains #nodes and length of shortest tour found by EAX
  tour.length = as.numeric(strsplit(lines[1], " ", fixed = TRUE)[[1]][2])
  tour = as.integer(strsplit(lines[2], " ", fixed = TRUE)[[1]])

  # cleanup
  unlink(paste(args$tour.file, "_BestSol", sep = ""))
  unlink(paste(args$tour.file, "_Result", sep = ""))
  #}
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
