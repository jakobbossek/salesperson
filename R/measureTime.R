#' Helper for measuring time.
#'
#' Simple wrapper around \code{proc.time}.
#' @param expr [\code{expression}]\cr
#'   Expression the time should be measured.
#' @param feature.set.name [\code{character(1)}]\cr
#'   Name of the corresponding feature set. Used as a prefix for the cost feature.
#' @param append.as.feature [\code{logical(1)}]\cr
#'   Should the computational effort in seconds be saved as an additional feature?
#'   Default is \code{FALSE}.
#' @param envir [\code{environment}]\cr
#'   Environment in which expr should be evaluated.
#' @return Return value of evaluated \code{expr} with additional attribute
#'   \dQuote{time_elapsed}.
#' @export
measureTime = function(expr, feature.set.name, append.as.feature = FALSE, envir = parent.frame()) {
  start = proc.time()
  feats = eval(expr, envir = envir)
  time.elapsed = (proc.time() - start)[3]
  names(time.elapsed) = NULL
  if (append.as.feature) {
    feats[[paste(feature.set.name, "costs", sep = "_")]] = time.elapsed
  }
  return(feats)
}
