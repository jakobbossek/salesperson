# Extracts tour of tsplib file.
#
# @param file.path [character(1)]
#   Path to TSPlib tour file.
# @return [integer]
#   Permutation of the nodes.
#FIXME: move to 'salesperson package'.
readTSPlibTOURFile = function(file.path) {
    assertFile(file.path, access = "r")
    con = file(file.path, "r")
    on.exit(close(con))

    lines = readLines(con)
    #FIXME: actually we do not need the specifications. We could skip that.
    obj = list()
    i = 1L
    while (str_detect(lines[i], ":")) {
        spec = unlist(strsplit(lines[i], "[[:space:]]*:[[:space:]]*"))
        obj[[spec[1]]] = spec[2]
        i = i + 1L
    }
    i = i + 1L # skip TOUR_SECTION
    tour = numeric()
    #INFO: LKH tours contain the negative of the first node ID as the last element
    while (lines[i] != "EOF" && as.numeric(lines[i]) > 0) {
        tour = c(tour, as.integer(lines[i]))
        i = i + 1L
    }
    return(tour)
}

# Returns all currently available solver names.
#
# @return [character]
getAvailableSolverNames = function() {
    c("eax", "eax-restart", "lkh", "lkh-restart")
}

# Check if vector contains permutation of number 1:n
#
# @param x [numeric]
#   Vector.
# @param source [numeric]
#   Vector which we want to compare with. Default is 1, ..., length(x).
# @return [logical(1)]
isPermutation = function(x, source = seq(length(x))) {
    all(source == sort(x))
}

# Print TSPSolverResult to stdout.
#
# @param x [TSPSolverResult]
print.TSPSolverResult = function(x) {
    if (!is.null(x$error)) {
        catf("Instance '%s' could not be solved due to an error!", x$instance.name)
        catf("Error message: %s", as.character(x$error))
    } else {
        catf("Solved instance '%s' successfully!", x$instance.name)
        catf("Used solver:  %s", toupper(x$solver))
        catf("Elapsed time: %.2f [seconds]", x$runtime)
        catf("Tour length:  %.2f", x$tour.length)
        max.idx = min(length(x$tour), 10L)
        catf("Head of tour: %s", paste(collapse(x$tour[1:max.idx], sep = ", "), ", ...", sep = ""))
    }
}

# Run solver on instance.
#
# @param instance [Network]
#   Network.
# @param solver [character(1)]
#   Name of solver to use.
# @param ... [any]
#   Further parameters for the chosen solver. Not used.
# @return [TOUR]
#   TSP tour object.
runTSPSolver = function(instance, solver, ...) {
    # sanity checks
    assertCharacter(instance, len = 1L, any.missing = FALSE)
    assertChoice(solver, choices = getAvailableSolverNames())

    # dispatching
    start.time = proc.time()

    if (solver %in% c("eax", "eax-restart")) {
        res = runEAXSolver(instance, solver, ...)
    } else if (solver %in% c("lkh", "lkh-restart")) {
        res = runLKHSolver(instance, solver, ...)
    }

    end.time = proc.time()
    runtime = (end.time - start.time)[3]

    makeTSPSolverResult(
        instance.name = instance,
        solver = solver,
        tour.length = if (!is.null(res$tour.length)) res$tour.length else NA,
        tour = if (!is.null(res$tour)) res$tour else NA,
        runtime = runtime
    )
}

# Run LKH specific stuff.
# @interface see runTSPSolver
runLKHSolver = function(instance, solver, ...) {
    #FIXME: we need to make this more general. Since shipping binaries is not
    # allowed in R packages, the user needs to download the binary by hand and
    # set the correct path. Maybe do this via R options? This way it would
    # be sufficient to set options only once and not every time we use the solver.
    #FIXME: LKH-1.3 support really neccessary?
    #FIXME: parse LKH output. We need the tour length!
    if (solver == "lkh") {
        lkh.bin = "/Users/jboss/repositories/git/salesperson/bin/lkh-2.0.7/osx/lkh"
    } else {
        lkh.bin = "/Users/jboss/repositories/git/salesperson/bin/lkh-2.0.7-restart/osx/lkh"
    }
    param.file = paste(instance, ".par", sep="")
    lkh.args = c(param.file, 9999999)

    # Write specific parameter file (deleted later)
    # $ in the output file name is replaced by tour length by LKH (see USER GUIDE)
    output.file = paste(instance, ".out", sep = "")
    write(c(paste("PROBLEM_FILE =", instance), paste("OUTPUT_TOUR_FILE =", output.file), "RUNS = 1", "SEED = 1", "MAX_TRIALS = 100000000"), file = param.file)
    res = suppressWarnings(system2(lkh.bin, lkh.args, stdout = TRUE))

    # build tour
    tour = as.integer(readTSPlibTOURFile(output.file))

    #FIXME: parse results, extract relevent information
    x = paste(res)

    # cleanup
    unlink(param.file)

    return(list("tour" = tour, "error" = NULL))
}

# Run EAX specific stuff.
# @interface see runTSPSolver
runEAXSolver = function(instance, solver, ...) {
    #FIXME: we need to make this more global, i.e., maybe 'TSP::concorde_path' like
    if (solver == "eax") {
        eax.bin = "/Users/jboss/repositories/git/salesperson/bin/eax/osx/eax"
    } else {
        eax.bin = "/Users/jboss/repositories/git/salesperson/bin/eax-restart/osx/eax"
    }
    #temp.file = tempfile("EAX_") #FIXME: does not work as expected
    temp.file = paste(instance, ".out", sep = "")
    eax.args = c(1, temp.file, 100, 30, instance, 0, 3)
    if (solver == "eax-restart") {
        eax.args = c(eax.args, 1)
    }
    res = suppressWarnings(system2(eax.bin, eax.args, stdout = TRUE))
    #FIXME: how to check whether algo was successful?
    best.sol.conn = file(paste(temp.file, "_BestSol", sep = ""))
    lines = readLines(best.sol.conn)

    # extract relevant data
    # first line contains #nodes and length of shortest tour found by EAX
    tour.length = as.numeric(strsplit(lines[1], " ", fixed = TRUE)[[1]][2])
    tour = as.integer(strsplit(lines[2], " ", fixed = TRUE)[[1]])

    # cleanup
    unlink(paste(temp.file, "_BestSol", sep = ""))
    unlink(paste(temp.file, "_Result", sep = ""))

    return(list("tour" = tour, "tour.length" = tour.length, error = NULL))
}
