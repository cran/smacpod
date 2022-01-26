#' Difference of estimated K functions
#'
#' \code{kdest} computes the difference in estimated K functions for a set of
#' cases and controls, with \code{KD(r) = K_case(r) - K_control(r)} denoting the
#' estimated difference at distance \code{r}. If \code{nsim > 0}, then pointwise
#' tolerance envelopes for \code{KD(r)} are constructed under the random
#' labeling hypothesis for each distance \code{r}. The \code{summary} function
#' can be used to determine the distances for which \code{KD(r)} is above or
#' below the tolerance envelopes. The \code{plot} function will plot
#' \code{KD(r)} versus r, along with the tolerance envelopes, the min/max
#' envelopes of \code{KD(r)} simulated under the random labeling hypothesis, and
#' the average KD(r) under the random labeling hypothesis.
#'
#' This function relies internally on the \code{\link[spatstat.core]{Kest}} and
#' \code{\link[spatstat.core]{eval.fv}} functions from the \code{spatstat}
#' package.  The arguments are essentially the same as the
#' \code{\link[spatstat.core]{Kest}} function, and the user is referred there
#' for more details about the various arguments.
#' 
#' @inheritParams logrr   
#' @inheritParams spatstat.core::Kest
#'   
#' @return Returns a \code{kdenv} object.  See documentation
#'   for \code{spatstat::Kest}.
#' @author Joshua French
#' @export
#' @seealso \code{\link[spatstat.core]{Kest}},
#'   \code{\link[spatstat.core]{eval.fv}}
#' @references Waller, L.A. and Gotway, C.A. (2005). 
#'   Applied Spatial Statistics for Public Health Data. 
#'   Hoboken, NJ: Wiley.
#' @examples 
#' data(grave)
#' # estimate and plot KD(r)
#' kd1 = kdest(grave, case = "affected")
#' plot(kd1, iso ~ r, ylab = "difference", legend = FALSE, main = "")
#' kd2 = kdest(grave, case = 2, nsim = 9, level = 0.8)
#' kd2 # print object
#' summary(kd2) # summarize distances KD(r) outside envelopes
#' plot(kd2)
#' # manually add legend
#' legend("bottomright", legend = c("obs", "avg", "max/min env", "95% env"),
#'        lty = c(1, 2, 1, 2), col = c("black", "red", "darkgrey", "lightgrey"),
#'        lwd = c(1, 1, 10, 10))
kdest = function(x, case = 2, nsim = 0, level = 0.95, r = NULL, 
                 rmax = NULL, breaks = NULL, 
                 correction = c("border", "isotropic", "Ripley", "translate"), 
                 nlarge = 3000, domain = NULL, 
                 var.approx = FALSE, ratio = FALSE) {
  x = arg_check_ppp_marks(x)
  case = arg_check_case(case, x)
  arg_check_nsim(nsim)
  arg_check_level(level)

  if (nsim == 0) {
    out = kd(x, case = case, 
             r = r, rmax = rmax, breaks = breaks, 
             correction = correction, 
             nlarge = nlarge, domain = domain, 
             var.approx = var.approx, ratio = ratio)
    out = list(out = out)
  } else {
    #min/max envelope
    out = spatstat.core::envelope(x, kd, case = case, nsim = nsim, 
                             savefuns = TRUE, 
                             simulate = expression(spatstat.random::rlabel(x, permute = TRUE)), 
                             r = r, rmax = rmax, 
                             breaks = breaks, 
                             correction = correction, 
                             nlarge = nlarge, 
                             domain = domain, 
                             var.approx = var.approx, 
                             ratio = ratio)
    simfuns <- as.data.frame(attr(out, "simfuns"))
    simfuns[,1] <- out$obs
    l = apply(simfuns, 1, stats::quantile, prob  = (1 - level)/2)
    u = apply(simfuns, 1, stats::quantile, prob = 1 - (1 - level)/2)
    out = list(out = out, qlo = l, qhi = u)
  }
  out$r = out$out$r
  out$case_label = levels(x$marks)[case]
  out$control_label = levels(x$marks)[-case]
  out$nsim = nsim
  out$level = level
  out$rlim = range(out$r)
  class(out) = "kdenv"
  return(out)
}
