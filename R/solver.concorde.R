#' @export
makeTSPSolver.concorde = function() {
  makeTSPSolverInternal(
    cl = "concorde",
    short.name = "concorde",
    name = "Exect CONCORDE solver (sophisticated branch and cut ILP solver)",
    properties = c("euclidean", "deterministic", "exact", "external", "requires.tsplib"),
    #FIXME: concorde has a huge number of parameters. Are there any interesting ones?
    par.set = makeParamSet()
  )
}

#' @export
prepareInstance.concorde = function(solver, instance) {
  return(prepareInstance.eax(solver, instance))
}

#' @export
run.concorde = function(solver, instance, control) {
  x = importFromTSPlibFormat(instance)

  #FIXME: result file is located in working dir and is "INSTANCE_FILE.res".
  # SOme more result files are created additionally. We need to handle that correctly.
  temp.dir = tempdir()
  temp.file = tempfile(tmpdir = temp.dir)
  tour.file = paste0(temp.file, ".sol")
  result.file = paste0(temp.file, ".res")
  input.file = instance
  seed = coalesce(control$seed, 1L)

  # set arguments
  args = c(
    # output file
    "-o", tour.file,
    # random seed
    "-s", seed,
    # input file
    input.file
  )

  # Invoke ./concorde to get a list of all possible arguments.
  res = system2(solver$bin, args = args, stdout = TRUE, stderr = TRUE)

  # check for possible errors
  if (hasAttributes(res, "status")) {
    stopf("Error during concorde invocation.")
  }
  if (!file.access(tour.file) == 0) {
    stopf("Concorde output file could not be opened.")
  }

  # extract tour
  tour = scan(tour.file, what = integer(0), quiet = TRUE)

  # the first line contains the number of nodes. Thus we delete the first element.
  # Moreover we need to add 1 to each node, since the enumeration starts with 0.
  tour = tour[-1] + 1L

  # extract tour length
  # The first line in the result file contains the tour length as the second entry
  #FIXME: occasionally concorde outputs a *.res file from which the length could
  #be extracted. However, this is not always the case :-/
  #tour_lengh = as.integer(read.table(result_file, nrows = 1L))$V3
  #FIXME: really make sure, that this is correct!
  tour_length = computeTourLength(x, tour, round = TRUE)

  # cleanup
  unlink(c(temp.file, tour.file, result.file))

  return(list(tour = tour, tour.length = tour_length, error = NULL, solver.output = res))
}
