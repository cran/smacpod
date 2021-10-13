#' Global test of clustering using difference in K functions
#'
#' \code{kdplus.test} performs a global test of clustering
#' for comparing cases and controls using the method of
#' Diggle and Chetwynd (1991).  It relies on the difference
#' in estimated K functions.
#'
#' @param x A \code{kdenv} object from the \code{kdest}
#'   function.
#'
#' @return A list providing the observed test statistic
#'   (\code{kdplus}) and the estimate p-value
#'   {\code{pvalue}}.
#' @author Joshua French
#' @seealso \code{\link{kdest}}
#' @export
#' @references Waller, L.A. and Gotway, C.A. (2005).
#'   Applied Spatial Statistics for Public Health Data.
#'   Hoboken, NJ: Wiley.  
#'   
#'   Diggle, Peter J., and Amanda G.
#'   Chetwynd. "Second-order analysis of spatial clustering
#'   for inhomogeneous populations." Biometrics (1991):
#'   1155-1163.
#' @examples
#' data(grave)
#' # construct envelopes for differences in estimated K functions
#' kdenv = kdest(grave, nsim = 9)
#' kdplus.test(kdenv)
kdplus.test = function(x) {
  if (max(class(x) == "kdenv") < 1) stop("x must be an object from the kdenv function.")
  simfuns <- as.data.frame(attr(x[[1]], "simfuns"))
  simfuns[,1] <- x[[1]]$obs # replace r with obs kd
  sdkdhat = apply(simfuns, 1, stats::sd) # estimated variance of kdest simulations
  # turn into matrix
  sdmat = matrix(sdkdhat, nrow = nrow(simfuns), ncol = ncol(simfuns))
  # estimate KD + for simulated data
  kdplussim = colSums(simfuns/sdmat, na.rm = TRUE)
  # determine proportion of simulated KD+ and observed KD+
  # greater than KD+
  p = mean(kdplussim >= kdplussim[1])
  # cat(paste("The p-value for the global test is", round(p, 3)))
  structure(list(statistic = kdplussim[1], null_statistics = kdplussim[-1],
                 pvalue = p,
                 nsim = (length(kdplussim) - 1),
                 case_label = x$case_label,
                 control_label = x$control_label,
                 rlim = x$rlim),
            class = "kdplus_test")
}

#' Print a \code{kdplus_test} object
#'
#' Print a \code{kdplus_test} object produced by 
#' \code{\link[smacpod]{kdplus.test}}.
#'
#' @param x An object produced by the \code{\link[smacpod]{kdplus.test}} function.
#' @param ... Not currently implemented.
#' @return Information about the test
#' @author Joshua French
#' @export
print.kdplus_test = function(x, ...) {
  cat("\n")
  cat("Diggle and Chetwynd (1991) test for difference in K functions\n")
  cat("\n")
  cat("KD(r) = K_case(r) - K_control(r)\n")
  cat("case label: ", x$case_label, "\n")
  cat("control label: ", x$control_label, "\n")
  cat("\n")
  cat("null hypothesis: KD(r) = 0 for all r between", x$rlim[1], "and", x$rlim[2], "\n")
  cat("alternative hypothesis: KD(r) > 0 for at least one r between",
      x$rlim[1], "and", x$rlim[2], "\n")
  cat("test statistic:", x$statistic,"\n")
  cat("p-value:", x$pvalue, "\n")
  cat("nsim:", x$nsim,"\n")
  cat("simulation procedure: random labeling\n")
}
