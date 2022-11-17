#' Print a \code{logrrenv} object
#'
#' Print a \code{logrrenv} object produced by
#' \code{\link[smacpod]{logrr}}.
#'
#' @param x An object produced by the
#'   \code{\link[smacpod]{logrr}} function.
#' @param ... Not currently implemented.
#' @return Information about the \code{logrrenv}
#' @author Joshua French
#' @export
print.logrrenv = function(x, ...) {
  cat(paste("\n",
            x$envelope,
            " tolerance envelopes for log relative risk, r(s)\n\n",
            sep = ""))
  cat("r(s) = ln[f(s)/g(s)]\n")
  cat("f(s) = spatial density of cases at location s\n")
  cat("g(s) = spatial density of controls at location s\n")
  cat("case label: ", x$case_label, "\n")
  cat("control label: ", x$control_label, "\n")
  cat("number of simulations:", x$nsim,"\n")
  cat("simulation procedure: random labeling\n")
  cat("envelope level: ", x$level, "\n")
}
