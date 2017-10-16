# Writes TSP tour to file in tour format.
#
# @param file.path [\code{character(1)}]\cr
#   Path to file the tour should be saved to.
# @param instance [\code{\link[netgen]{Network}}]\cr
#   TSP instance object.
# @param n [\code{integer(1)} | \code{NULL}]\cr
#   Instance size.
# @param name [\code{character(1)}]\cr
#   Name for the tour. Default is \dQuote{Some tour}.
# @param tour [\code{integer}]\cr
#   The actual tour, i.e., a permutation of the nodes of
#   \code{instance}.
# @return [\code{inivisble(logical(1))}]
#   Silently returns if save process was sucessful.
# @export
writeTSPlibTOURFile = function(file.path, instance = NULL, n = NULL, name = "Some tour", tour) {
  con = file(file.path, "w")
  # close connection on exit
  on.exit(close(con))

  if (is.null(instance) & is.null(n))
    stopf("Either the instance or n needs to be passed.")

  n = if (is.null(instance)) n else getNumberOfNodes(instance)

  # sanity check
  assertInteger(tour, len = n, any.missing = FALSE, all.missing = FALSE)
  assertString(name)

  writeLines(paste0("Name : ", name), con)
  writeLines("Type : TOUR", con)
  writeLines(paste0("DIMENSION : ", n), con)
  writeLines("TOUR_SECTION", con)
  tour = as.character(tour)
  #FIXME: I am sure there is a better way
  for (node in tour)
    writeLines(node, con)
  writeLines("-1", con)
  return(invisible(TRUE))
}
