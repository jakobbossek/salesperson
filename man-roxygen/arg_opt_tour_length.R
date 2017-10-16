#' @param opt.tour.length [\code{integer(1) | NULL}]\cr
#'   Length of an optimal TSP tour. This is only relevant for benchmarking.
#'   Keep in mind, that in case of a given optimal tour length most internal
#'   stopping criteria of EAX are deacitvated. The algorithm stops if an optimal
#'   tour is found or the cutoff time is reached.
