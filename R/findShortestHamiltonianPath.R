#' @title
#' Shortest Hamiltonian Path computation.
#'
#' @description
#' Find the Shortest Hamiltonian Path between two cities by
#' means of a TSP formulation.
#'
#' @note
#' At the moment the function only works correctly if start.id = 1 and dest.id = 2
#' or vice versa and the input graph has exactly two depots.
#'
#' @param x [Network]
#'   Network (see package netgen).
#' @param start.id [integer(1)]
#'   ID of the start node.
#' @param dest.id [integer(1)]
#'   ID of the end node.
#' @return [integer]
#'   Shortest hamiltonian path.
#' @export
findShortestHamiltonianPath = function(x, start.id, dest.id) {
  #FIXME: add sanity checks
  if (!testClass(x, "Network"))
    stopf("We need a network (see package netgen).")

  if (!hasDepots(x) & getNumberOfDepots(x) != 2)
    stopf("We need a network with exactly two depots.")

  #FIXME: recomputation. We need to fix this in netgen!
  # compute pairwise distances
  coords = as.matrix(as.data.frame(x, include.extra = FALSE))

  # compute pairwise distances
  dist.mat = as.matrix(dist(coords))

  # depots are always stored as the first two cities/nodes.
  # remove this nodes from the distance matrix and generate ATSP
  atsp = TSP::ATSP(dist.mat[-c(start.id, dest.id), -c(start.id, dest.id)])

  # insert dummy city with label sd (sd for start/dest) ...
  atsp = TSP::insert_dummy(atsp, label = "sd")
  sd.id = which(labels(atsp) == "sd")

  # ... and set outgoing distances from the dummy node to the
  # outgoing distances of the start node and the incoming distances of the
  # dummy node to the distances of the destination node
  atsp[sd.id, ] = c(dist.mat[-c(start.id, dest.id), start.id], 0)
  atsp[, sd.id] = c(dist.mat[dest.id, -c(start.id, dest.id)], 0)

  # reformulate stuff as an STSP problem, i.e., double the number of nodes
  stsp = TSP::reformulate_ATSP_as_TSP(atsp)

  # solve STSP with concorde
  TSP::concorde_path(path = solverPaths()$concorde)
  stsp.tour = TSP::solve_TSP(stsp, method = "concorde")

  # extract ATSP tour from STSP SOLUTION
  atsp.tour = TSP::as.TOUR(stsp.tour[stsp.tour <= TSP::n_of_cities(atsp)])

  #FIXME: this works only if start.id and dest.id are in {1,2}.
  # Add start and end tour
  # NOTE! NOTE! NOTE!
  # If the dummy nodes appear before and not after the original nodes
  # in the solution of the STSP, we need to reverse the tour! This is done
  # here simply be taking the shorter tour (reversed vs original).
  # (See paper about R package TSP by Hahsler and Hornik)
  h.path1 = c(2L, TSP::cut_tour(atsp.tour, sd.id) + 2L, 1L)
  h.path2 = c(1L, TSP::cut_tour(atsp.tour, sd.id) + 2L, 2L)

  h.path1.length = salesperson::computeTourLength(x, h.path1, close.tour = FALSE)
  h.path2.length = salesperson::computeTourLength(x, h.path2, close.tour = FALSE)

  if (h.path1.length < h.path2.length) {
    return(h.path1)
  }
  return(h.path2)
}
