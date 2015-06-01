# Returns all currently available solver names.
#
# @return [\code{character}]
# @export
getAvailableSolverNames = function() {
  c("eax", "eax-restart", "lkh", "lkh-restart", "concorde")
}
