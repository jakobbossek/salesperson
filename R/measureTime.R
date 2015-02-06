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
    end = proc.time() - start
    attr(feats, "time_elapsed") = end[3]
    return(feats)
}