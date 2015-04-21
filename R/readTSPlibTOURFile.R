# Extracts tour from TSPlib file.
#
# Since we need only the tour and the tour length, this functions skips the
# specification part.
#
# @param file.path [\code{character(1)}]\cr
#   Path to TSPlib tour file.
# @return [\code{integer}]
#   Permutation of the nodes.
# @export
readTSPlibTOURFile = function(file.path) {
  assertFile(file.path, access = "r")
  con = file(file.path, "r")

  # close connection on exit
  on.exit(close(con))

  #FIXME: actually we do not need the specifications. We could skip that.
  lines = readLines(con)
  obj = list()
  i = 1L
  while (stringr::str_detect(lines[i], ":")) {
    spec = unlist(strsplit(lines[i], "[[:space:]]*:[[:space:]]*"))
    obj[[spec[1]]] = spec[2]
    i = i + 1L
  }
  # skip TOUR_SECTION line
  i = i + 1L

  tour = numeric()
  #INFO: LKH tours contain the negative of the first node ID as the last element
  while (lines[i] != "EOF" && as.numeric(lines[i]) > 0) {
    tour = c(tour, as.integer(lines[i]))
    i = i + 1L
  }
  return(tour)
}
