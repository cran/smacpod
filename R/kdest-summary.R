#' Summarize a \code{kdenv} object
#'
#' Summarize the sequences of distances for which the difference in estimated K
#' functions, \code{KD(r) = K_case(r) - K_control(r)}, falls outside the
#' non-rejection envelopes.
#' 
#' @param object An object produced by the \code{\link[smacpod]{kdest}}
#'   function.
#' @param ... Not currently implemented.
#' @return A list that contains the sequences of indices for which the estimated
#'   difference in KD functions is above the envelopes, below the envelopes, and
#'   the vector of distances.
#' @author Joshua French
#' @export
summary.kdenv = function(object, ...) {
  # idx of KD(r) > upper limit
  idx_hi = which(object$out$obs > object$qhi)
  # split into ranges of consecutive indices
  ranges_hi <- split(idx_hi, cumsum(c(1, diff(idx_hi) != 1)))
  # idx of KD(r) < lower limit
  idx_lo = which(object$out$obs < object$qlo)
  ranges_lo <- split(idx_lo, cumsum(c(1, diff(idx_lo) != 1)))
  # return findings
  summary_kdenv_list <- 
    list(ranges_hi = ranges_hi,
         ranges_lo = ranges_lo,
         r = object$r)
  class(summary_kdenv_list) = "kdenv_summary"
  return(summary_kdenv_list)
}

#' Print a \code{kdenv_summary} object
#'
#' @param x An object produced by \code{\link[smacpod]{summary.kdenv}}.
#' @param ... Not currently implemented.
#' @return Print summary
#' @author Joshua French
#' @export
print.kdenv_summary = function(x, ...) {
  r = x$r
  ranges_hi = x$ranges_hi
  ranges_lo <- x$ranges_lo
  
  # if KD(r) > envelope for at least one r
  if (length(ranges_hi[[1]]) >= 1) {
    cat("KD(r) > upper envelope limit for the following r:\n")
    
    for (i in seq_along(ranges_hi)) {
      if (length(ranges_hi[[i]]) > 1) {
        nrh <- length(ranges_hi[[i]])
        cat(r[ranges_hi[[i]][1]], "to", r[ranges_hi[[i]][nrh]], "\n")
      } else {
        cat(r[ranges_hi[[i]][1]],"\n")
      }
    }
  }
  
  # if KD(r) < envelope for at least one r
  if (length(ranges_lo[[1]]) >= 1) {
    cat("KD(r) < lower envelope limit for the following r:\n")
    for (i in seq_along(ranges_lo)) {
      if (length(ranges_lo[[i]]) > 1) {
        nrh <- length(ranges_lo[[i]])
        cat(r[ranges_lo[[i]][1]], "to", r[ranges_lo[[i]][nrh]], "\n")
      } else {
        cat(r[ranges_lo[[i]][1]],"\n")
      }
    }
  }
  
  if (length(ranges_lo[[1]]) == 0 & length(ranges_hi[[1]]) == 0) {
    cat("KD(r) within envelopes for all r considered")
  }
}
