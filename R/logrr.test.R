#' Global test of clustering using log ratio of spatial
#' densities
#'
#' \code{logrr.test} performs a global test of clustering
#' for comparing cases and controls using the log ratio of
#' spatial densities based on the method of Kelsall and
#' Diggle (1995).
#'
#' @param x An \code{logrrenv} object from the
#'   \code{\link{logrr}} function.
#'
#' @return A list providing the observed test statistic
#'   (\code{islogrr}) and the estimated p-value
#'   (\code{pvalue}).
#' @author Joshua French
#' @export
#' @references Waller, L.A. and Gotway, C.A. (2005). Applied
#'   Spatial Statistics for Public Health Data. Hoboken, NJ:
#'   Wiley.
#'
#'   Kelsall, Julia E., and Peter J. Diggle. "Non-parametric
#'   estimation of spatial variation in relative risk."
#'   Statistics in Medicine 14.21-22 (1995): 2335-2342.
#' @examples
#' data(grave)
#' logrrenv = logrr(grave, nsim = 9)
#' logrr.test(logrrenv)
logrr.test = function(x) {
  if (!is.element("logrrenv", class(x))) {
    stop("x must be an object from the logrr function")
  }
  
  win = x$window
  dim3 = dim(x$simr)[3]
  tsim = numeric(dim3)
  for (i in 1:dim3) {
   x$v = x$simr[,,i]^2
   tsim[i] = spatstat.geom::integral.im(x)
  }
  tobs = tsim[1]
  pvalue = mean(tsim >= tobs)
  
  structure(list(statistic = tobs,
                 null_statistics = tsim[-1],
                 pvalue = pvalue,
                 nsim = (length(tsim) - 1),
                 case_label = x$case_label,
                 control_label = x$control_label),
            class = "logrr_test")
}

#' Print a \code{logrr_test} object
#'
#' Print an \code{logrr_test} object produced by
#' \code{\link[smacpod]{logrr.test}}.
#'
#' @param x An object produced by the \code{\link[smacpod]{logrr.test}} function.
#' @param ... Not currently implemented.
#' @return Information about the test
#' @author Joshua French
#' @export
print.logrr_test = function(x, ...) {
  cat("\n")
  cat("Kelsall and Diggle (1995) test for log relative risk\n")
  cat("\n")
  cat("r(s) = ln[f(s)/g(s)]\n")
  cat("f(s) = spatial density of cases at location s\n")
  cat("g(s) = spatial density of controls at location s\n")
  cat("case label: ", x$case_label, "\n")
  cat("control label: ", x$control_label, "\n")
  cat("\n")
  cat("null hypothesis: r(s) = 0 for all s in study area\n")
  cat("alternative hypothesis: r(s) != 0 for at least one s in study area\n")
  cat("test statistic:", x$statistic,"\n")
  cat("p-value:", x$pvalue, "\n")
  cat("nsim:", x$nsim,"\n")
  cat("simulation procedure: random labeling\n")
}