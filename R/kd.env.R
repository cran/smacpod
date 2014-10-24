#' Envelopes for difference of estimated K functions
#' 
#' \code{kd.env} determines envelopes for the difference in estimated K functions for a set of cases and controls.  By default, produces the min/max envelopes.  Additional confidence envelopes can be obtained using the \code{confint} function.
#' 
#' This function relies internally on the \code{Kest}, \code{eval.fv}, and \code{envelope.fv} functions from the \code{spatstat} package.  The arguments are essentially the same as the \code{Kest} function.  See the documentation of the \code{Kdest} for more details about the various arguments.
#' 
#' @param x A \code{ppp} object from the \code{spatstat} package with marks for the case and control groups.
#' @param case The position of the name of the "case" group in levels(x$marks).  The default is 2.
#' @param nsim The number of simulated data sets from which to construct the envelopes under the random labeling hypothesis.
#' @param r Optional. Vector of values for the argument r at which K(r) should be evaluated. Users are advised not to specify this argument; there is a sensible default.
#' @param breaks This argument is for internal use only.
#' @param correction Optional. A character vector containing any selection of the options "none", "border", "bord.modif", "isotropic", "Ripley", "translate", "translation", "none", "good" or "best". It specifies the edge correction(s) to be applied.
#' @param nlarge Optional. Efficiency threshold. If the number of points exceeds nlarge, then only the border correction will be computed (by default), using a fast algorithm.
#' @param domain Optional. Calculations will be restricted to this subset of the window. See Details.
#' @param var.approx  Logical. If TRUE, the approximate variance of Kest(r) under CSR will also be computed.
#' @param ratio	Logical. If TRUE, the numerator and denominator of each edge-corrected estimate will also be saved, for use in analysing replicated point patterns.
#'
#' @return Returns an \code{fv} object.  See documentation for \code{envelope} function in \code{spatstat} package.  Can be plotted using \code{plot.envelope}.  Additionall see \code{plot.fv}.
#' @author Joshua French
#' @import spatstat
#' @export
#' @references Waller, L.A. and Gotway, C.A. (2005).  Applied Spatial Statistics for Public Health Data.  Hoboken, NJ: Wiley.  Kulldorff, M. (1997) A spatial scan statistic. Communications in Statistics -- Theory and Methods 26, 1481-1496.
#' @examples 
#' data(grave)
#' env = kd.env(grave, nsim = 19)
#' plot(env, legend = FALSE, main = "")
kd.env = function(x, case = 2, nsim = 99, r=NULL, breaks=NULL, correction=c("border", "isotropic", "Ripley", "translate"), nlarge=3000, domain=NULL, var.approx=FALSE, ratio=FALSE)
{
  out = envelope(x, kdest, nsim = nsim, savefuns = TRUE, 
                 simulate = expression(rlabel(x, permute = TRUE)), 
                 r = r, breaks = breaks, correction = correction, 
                 nlarge = nlarge, domain = domain, 
                 var.approx = var.approx, ratio = ratio)
  class(out) <- c(class(out), "kdenv")
  return(out)
}


