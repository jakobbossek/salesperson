# Generate result object.
#
# @param instance.name [\code{character(1)}]\cr
#   Instance name.
# @param solver [\code{character(1)}]\cr
#   Solver used to solve instance.
# @param tour.length [\code{numeric(1)} | \code{NA}]\cr
#   Length of the shortest tour found or NA if unknown.
# @param tour [\code{integer}]\cr
#   Tour, i.e., permutation of the node IDs.
# @param runtime [\code{numeric(1)}]\cr
#   Actual runtime of the solver on the instance.
# @param error [\code{character(1)}]\cr
#   Error message in case of solver failing on instance.
# @return [\code{TSPSolverResult}]
#   Result object which contains
#   \describe{
#     \item{instance.name}{Name of the instance solved.}
#     \item{solver}{Solver name used to solve the instance.}
#     \item{tour.length}{Tour length}
#     \item{tour}{Permutation of the nodes.}
#     \item{runtime}{Running time measured via \code{proc.time}}
#     \item{error}{Error message, which occured during optimization.}
#   }
# @export
makeTSPSolverResult = function(instance.name, solver,
  tour.length = NA, tour = NA, runtime = NA, error = NULL) {
    assertCharacter(instance.name, len = 1L)
    assertCharacter(solver, len = 1L)
    !is.na(tour.length) && assertNumber(tour.length, na.ok = FALSE)
    !is.na(tour) && assertInteger(tour, min.len = 1L, lower = 1L, any.missing = FALSE)
    !is.na(runtime) && assertNumber(runtime, na.ok = FALSE)
    if (!isPermutation(tour)) {
        stopf("Passed tour is not a permutation of the nodes!")
    }
    makeS3Obj(
        instance.name = instance.name,
        solver = solver,
        tour.length = tour.length,
        tour = tour,
        runtime = runtime,
        error = error,
        classes = "TSPSolverResult"
    )
}
