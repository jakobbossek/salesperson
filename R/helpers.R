#' Removes the file extension of a given filename.
#'
#' @param filename [character(1)]
#'   Filename.
#' @return Truncated filename, i.e., filename without last file extension.
removeFileExtension = function(filename) {
  # split by path
  x = str_split(filename, pattern = "\\.")
  # since we only provide a single filename, we select the first (and only) list element
  x = x[[1]]
  n = length(x)
  collapse(x[-n], sep = ".")
}

#' Compute tour length given a TSP instance and a tour/permutation.
#'
#' @param x [\code{Network}]\cr
#'   TSP instance.
#' @param tour [\code{integer}]\cr
#'   Permutation of nodes.
#' @param round [\code{logical}]\cr
#'   Should the distances be rounded? Default is \code{FALSE}. Concorde internally
#'   does this.
#' @return [\code{numeric(1)}]
#' @export
computeTourLength = function(x, tour, round = FALSE) {
  assertClass(x, "Network")
  assertFlag(round)
  n = getNumberOfNodes(x)

  if (!testInteger(tour) || !isPermutation(tour, source = seq(n))) {
    stopf("Second parameter needs to be a permutation of {1, ..., %i}.", n)
  }

  # close tour
  tour = c(tour, tour[1])
  tour_length = 0

  # compute tour length
  for (i in 1:(length(tour) - 1L)) {
    tour_length = tour_length + x$distance.matrix[tour[i], tour[i + 1L]]
    if (round) {
      tour_length = round(tour_length)
    }
  }
  return(tour_length)
}

#' Extracts tour from TSPlib file.
#'
#' Since we need only the tour and the tour length, this functions skips the
#' specification part.
#'
#' @param file.path [\code{character(1)}]\cr
#'   Path to TSPlib tour file.
#' @return [\code{integer}]
#'   Permutation of the nodes.
#' @export
readTSPlibTOURFile = function(file.path) {
  assertFile(file.path, access = "r")
  con = file(file.path, "r")

  # close connection on exit
  on.exit(close(con))

  tour = numeric()
  tour.length = NA

  lines = readLines(con)
  obj = list()
  i = 1L
  while (stringr::str_detect(lines[i], ":")) {
    spec = unlist(strsplit(lines[i], "[[:space:]]*:[[:space:]]*"))
    if (stringr::str_detect(spec[2], "Length")) {
      # If there is a line COMMENT: Length = <tour-length>
      # extract the <tour-length>
      tour.length = as.numeric(unlist(strsplit(spec[2], "[[:space:]]*=[[:space:]]*"))[2])
    }
    obj[[spec[1]]] = spec[2]
    i = i + 1L
  }
  # skip TOUR_SECTION line
  i = i + 1L

  #INFO: LKH tours contain the negative of the first node ID as the last element
  while (lines[i] != "EOF" && as.numeric(lines[i]) > 0) {
    tour = c(tour, as.integer(lines[i]))
    i = i + 1L
  }
  return(list(tour = as.integer(tour), tour.length = tour.length))
}

#' Check if vector contains permutation of number 1:n
#'
#' @param x [\code{numeric}]\cr
#'   Vector to check.
#' @param source [\code{numeric}]\cr
#'   Vector which we want to compare with. Default is 1, ..., length(x).
#' @return [\code{logical(1)}]
isPermutation = function(x, source = seq(length(x))) {
  return(all(source == sort(x)))
}
