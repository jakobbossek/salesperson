context("TSP solvers")

test_that("Solvers from TSP package can be called without problems.", {
  # do five repetitions with random instances
  n.reps = 5L
  # check these solvers
  solvers = c("nearest_insertion", "farthest_insertion", "cheapest_insertion", "arbitrary_insertion",
    "nn", "repetitive_nn")

  for (i in seq(n.reps)) {
    n.points = sample(10:50, size = 1L)
    x = netgen::generateRandomNetwork(n.points)
    for (solver in solvers) {
      res = runSolver(solver, instance = x)
      expect_valid_TSPSolverResult(res, n.points, info = solver)
    }
  }
})
