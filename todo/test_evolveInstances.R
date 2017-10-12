context("Evolving TSP instances")

test_that("test evolveInstances: returns reasonable results", {
  max.iter = 5L
  n.points = 10L
  n.population = 3L

  fitness.fun = function(x) {
    x = netgen::makeNetwork(coordinates = x, name = "SomeTSPInstance")
    tour1 = runSolver("arbitrary_insertion", x)
    tour2 = runSolver("cheapest_insertion", x)
    return(tour1$tour.length / tour2$tour.length)
  }

  res = evolveInstances(
    fitness.fun = fitness.fun,
    n.population = n.population,
    n.offspring = 1L,
    stopping.conditions = list(setupMaximumIterationsTerminator(max.iter = max.iter)),
    monitor = NULL,
    n.points = n.points
  )
  expect_class(res, classes = c("ecr_result", "ecr_single_objective_result"))
  expect_class(res$opt.path, classes = c("OptPathDF", "OptPath"))
  expect_data_frame(as.data.frame(res$opt.path), nrows = (max.iter + 1L) * n.population, col.names = "unique")
  expect_true(res$best.value < 1.0)
  expect_matrix(res$best.param, ncols = 2L, nrows = n.points)
})
