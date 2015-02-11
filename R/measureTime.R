#' Helper for measuring time.
#'
#' Simple wrapper around \code{proc.time}.
#' @param expr [\code{expression}]\cr
#'   Expression the time should be measured.
#' @param envir [\code{environment}]\cr
#'   Environment in which expr should be evaluated.
#' @return Return value of evaluated \code{expr} with additional attribute
#'   \dQuote{time_elapsed}.
#' @export
measureTime = function(expr, envir = parent.frame()) {
    start = proc.time()
    feats = eval(expr, envir = envir)
    time.elapsed = proc.time() - start
    names(time.elapsed) = NULL
    attr(feats, "time.elapsed") = time.elapsed[3]
    return(feats)
}
