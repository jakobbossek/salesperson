#' @export
makeTSPSolver.concorde = function() {
  makeTSPSolverInternal(
    cl = "concorde",
    short.name = "concorde",
    name = "Exect CONCORDE solver (sophisticated branch and cut ILP solver)",
    properties = c("euclidean", "deterministic", "exact", "external", "requires.tsplib")
  )
}

#' @export
run.concorde = function(solver, instance, verbose = FALSE,
  initial.upperbound = NULL, cutoff.time = 0, ...) {
  # set concorde path
  assertFlag(verbose)

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
    exportToTSPlibFormat(instance, filename = file.input, full.matrix = FALSE, use.extended.format = FALSE)
  } else {
     file.input = instance
     # weird concorde behaviour: temp.file name is ignored
     # and files are named <instance_name>.{res,sol}
     temp.file = gsub(".tsp", "", basename(instance))
  }
  assertFile(file.input, "r")

  # build filenames for file which store the results
  file.output = paste0(temp.file, ".sol")
  file.res = paste0(temp.file, ".res")

  # Example call: ./concorde -x -o d657.sol d657.tsp
  args = list("-x", "-o", file.output)
  if (!is.null(initial.upperbound)) {
    args = c(args, list("-u", as.integer(initial.upperbound)))
  }
  args = c(args, list(file.input))

  # try to call solver
  solver.output = system2(solver$bin, args, stdout = TRUE, stderr = verbose, timeout = cutoff.time)

  # on timeout (see https://www.rdocumentation.org/packages/base/versions/3.6.1/topics/system2)
  if (attr(solver.output, "status") == 124) {
    return(list(error = "timeout"))
  }

  if (verbose)
    print(solver.output)

  # get tour
  n.nodes = scan(file.output, what = integer(), n = 1L, quiet = TRUE)
  tour = scan(file.output, what = integer(), skip = 1L, nmax = n.nodes, quiet = TRUE)
  tour = tour + 1L # since concorde is 0-based

  #tour.length = as.integer(scan(file.res, what = character(), n = 3L)[3L])
  # since concorde does obviously not always return a
  # .res file with the tour length we need to grep the output
  tour.length = grep("Optimal Solution: (.0-9)*", solver.output, value = TRUE)
  tour.length = gsub("Optimal Solution: ", "", tour.length)
  tour.length = as.integer(tour.length)

  # cleanup
  #unlink(c(file.output, file.res, paste0("O", file.res)))
  if (is.temp.input) {
    unlink(file.input)
  }

  list(
    tour = tour,
    tour.length = tour.length,
    trajectory = NULL,
    error = NULL,
    solver.output = solver.output
  )
}
