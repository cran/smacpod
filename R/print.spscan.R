#' Plots object from \code{\link{spscan.test}}.
#' 
#' Plots object of class \code{spscan} from
#' \code{\link{spscan.test}}.
#' 
#' If \code{border}, \code{ccol}, \code{clty}, or 
#' \code{clwd} are specified, then the length of these
#' vectors must match \code{nrow(x$coords)}.
#' 
#' @param x An object of class \code{spscan}.
#' @param ... Additional arguments affecting the summary produced.
#' @param extra A logical value. Default is \code{FALSE}.
#' \code{TRUE} indicates that extra information should be
#' printed.
#' @export
#' @examples
#' data(grave)
#' out = spscan.test(grave, case = 2, alpha = 0.1)
#' out
print.spscan = function(x, ..., extra = FALSE) {
  cat(paste(("method:"), (x$method)), sep = "\n")
  cat(paste("distance upperbound: ", x$maxd), sep = "\n")
  cat(paste(("realizations:"), (x$nsim)), sep = "\n")
  if (extra) {
    cat(paste(("significance level:"),
              (x$alpha)), sep = "\n")
    dtype = ifelse(x$longlat, "great circle", "euclidean")
    cat(paste(("distance metric:"),
              (dtype)), sep = "\n")
    cat(paste(("total events:"),
              (x$ppp$n)), sep = "\n")
    cat(paste(("total cases:"),
              round(x$total_cases, 2)), sep = "\n")    
}
  
}
