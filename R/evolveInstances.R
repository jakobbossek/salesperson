setupGeneratingEAMutator = function(
  normal.mutation.rate = 0.1,
  normal.mutation.sigma = 0.025,
  uniform.mutation.rate = 0.05,
  rnd.before.mutation = FALSE) {
  force(normal.mutation.rate)
  force(normal.mutation.sigma)
  force(uniform.mutation.rate)
  makeMutator(
    mutator = function(ind, task, control) {
      rnd = control$custom.constants$rnd
      cells.round = control$custom.constants$cells.round

      # global mutation (uniform)
      ind = mutateUniform(ind, uniform.mutation.rate)


      # local mutation (normal)
      if (!rnd) {
        offspring = mutateNormal(ind, normal.mutation.rate, normal.mutation.sigma)
      } else if(rnd.before.mutation) {
        ind = rescaleCoordinates(ind)
        ind = roundToGrid(ind, cells.round)
        ind = mutateNormal(ind, normal.mutation.rate, normal.mutation.sigma)
      } else {
        ind = mutateNormal(ind, normal.mutation.rate, normal.mutation.sigma)
        ind = rescaleCoordinates(ind)
        ind = roundToGrid(ind, cells.round)
      }

      return(ind)
    },
    name = "GeneratingEA: mutator",
    description = "Mutates instances by shifting points (local) and replacing points (global)."
  )
}

setupGeneratingEARecombinator = function(frac = 0.5) {
  force(frac)
  makeRecombinator(
    recombinator = function(inds, task, control) {
      parent1 = inds[[1L]]
      parent2 = inds[[2L]]
      n.points = nrow(parent1)

      # build offspring
      offspring = matrix(NA, ncol = 2L, nrow = n.points)
      idx = runif(n.points) < frac
      offspring[idx, ] = parent1[idx, ]
      offspring[!idx, ] = parent2[!idx, ]

      return(offspring)
    },
    name = "GeneratingEA: recombinator",
    description = "Takes 50% of each instance.",
    supported = "custom",
    n.children = 1L
  )
}

setupGeneratingEAUniformGenerator = function() {
  makeGenerator(
    generator = function(size, task, control) {
      n.points = control$custom.constants$n.points
      rnd = control$custom.constants$rnd
      cells.round = control$custom.constants$cells.round
      coordinates = matrix(runif(size * 2L * n.points), ncol = 2L)
      population = list()
      for (i in seq_len(size)) {
        population[[i]] = rescaleCoordinates(coordinates[((i - 1) * n.points + 1L):(i * n.points), ])
        if (rnd) {
          population[[i]] = roundToGrid(population[[i]], cells.round)
        }
      }
      return(makePopulation(population))
    },
    name = "GeneratingEA: random population generator",
    description = "Generates RUE instances in [0,1] x [0,1]",
    supported = "custom"
  )
}

#' @title
#' TSP generating EA.
#'
#' @description
#' Function to create instances for the Travelling Salesperson Problem or a
#' related orienteering problem by means of a sophisticated evololutionary
#' algorithm.
#'
#' @param fitness.fun [\code{function(x)}]\cr
#'   Fitness function used to judge the fitness of a TSP instance.
#'   \code{x} is a numeric matrix with 2 columns, containing
#'   the coordinates of a TSP instance.
#' @param n.population [\code{integer(1)}]\cr
#'   Number of TSP instances maintained in each population.
#'   Default is 30.
#' @param n.offspring [\code{integer(1)}]\cr
#'   Number of TSP instances generated in each evolutionary cycle.
#'   Default is \code{n.population}.
#' @param cells.round [\code{numeric(1)}]\cr
#'   Grid resolution for rounding.
#'   Default is 100.
#' @param rnd [\code{logical(1)}]\cr
#'   Round coordinates to grid centers?
#'   Default is \code{FALSE}.
#' @param n.points [\code{integer(1)}]\cr
#'   Number of cities of each TSP instance.
#'   Default is 50.
#' @param uniform.mutation.rate [\code{numeric(1)}]\cr
#'   Mutation probability in uniform mutation (in [0,1]).
#'   Only relevant if \code{mutator = NULL}, i.e., no custom mutator is set.
#' @param normal.mutation.rate [\code{numeric(1)}]\cr
#'   Mutation probability in normal mutation (in [0,1]).
#'   Only relevant if \code{mutator = NULL}, i.e., no custom mutator is set.
#' @param normal.mutation.sigma [\code{numeric(1)}]\cr
#'   Standard deviation of normal noise in normal mutation.
#'   Only relevant if \code{mutator = NULL}, i.e., no custom mutator is set.
#' @param rnd.before.mutation [\code{logical(1)}]\cr
#'   Round the coordinates before normal mutation.
#'   Default is \code{FALSE}.
#' @param ... [any]\cr
#'   Not used.
#' @inheritParams ecr::setupECRControl
#' @inheritParams ecr::setupEvolutionaryOperators
#' @return [\code{\link[ecr]{ecr_result}}]
#' @export
evolveInstances = function(
  fitness.fun,
  n.population = 30L,
  n.offspring,
  n.elite = 1L,
  survival.strategy = "plus",
  logger = ecr::setupOptPathLoggingMonitor(),
  cells.round = 100L,
  rnd = FALSE,
  n.points,
  normal.mutation.rate = 0.1,
  normal.mutation.sigma = 0.025,
  uniform.mutation.rate = 0.05,
  rnd.before.mutation = FALSE,
  parent.selector = ecr::setupTournamentSelector(k = 2L),
  survival.selector = ecr::setupGreedySelector(),
  recombinator = setupGeneratingEARecombinator(frac = 0.5),
  stopping.conditions = NULL,
  monitor = ecr::setupConsoleMonitor(),
  mutator = NULL,
  generator = setupGeneratingEAUniformGenerator()) {

  # build task
  if (!inherits(fitness.fun, "ecr_optimization_task")) {
    fitness.fun = ecr::makeOptimizationTask(
      fun = fitness.fun,
      n.objectives = 1L,
      minimize = TRUE
    )
  }

  # sanity check custom constants
  cells.round = asInt(cells.round, lower = 1L)
  assertFlag(rnd)
  n.points = asInt(n.points, lower = 5L)
  assertFlag(rnd.before.mutation)

  # create control object
  control = ecr::setupECRControl(
    n.population = n.population,
    n.offspring = n.offspring,
    representation = "custom",
    n.elite = n.elite,
    survival.strategy = survival.strategy,
    stopping.conditions = stopping.conditions,
    logger = logger,
    monitor = monitor,
    custom.constants = list(
      cells.round = cells.round,
      rnd = rnd,
      n.points = n.points
    )
  )

  # if user did not pass a custom mutator, fallback to our default
  mutator = if (is.null(mutator)) {
    setupGeneratingEAMutator(
      normal.mutation.rate = normal.mutation.rate,
      normal.mutation.sigma = normal.mutation.sigma,
      uniform.mutation.rate = uniform.mutation.rate,
      rnd.before.mutation = rnd.before.mutation
    )
  }

  # set up evolutionary operators
  control = ecr::setupEvolutionaryOperators(
    control,
    parent.selector = parent.selector,
    survival.selector = survival.selector,
    mutator = mutator,
    recombinator = recombinator,
    generator = generator
  )

  ecr::doTheEvolution(fitness.fun, control = control)
}
