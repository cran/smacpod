#' Difference of estimated K functions
#' 
#' \code{kdest} determines the difference in estimated K functions for a set of cases and controls.
#' 
#' This function relies internally on the \code{Kest} and \code{eval.fv} functions from the \code{spatstat} package.  So the arguments are essentially the same as the \code{Kest} function.  See the documentation of the \code{Kdest} for more details about the various arguments.
#' 
#' @param x A \code{ppp} object from the \code{spatstat} package with marks for the case and control groups.
#' @param case The position of the name of the "case" group in levels(x$marks).  The default is 2.
#' @param r Optional. Vector of values for the argument r at which K(r) should be evaluated. Users are advised not to specify this argument; there is a sensible default.
#' @param breaks This argument is for internal use only.
#' @param correction Optional. A character vector containing any selection of the options "none", "border", "bord.modif", "isotropic", "Ripley", "translate", "translation", "none", "good" or "best". It specifies the edge correction(s) to be applied.
#' @param nlarge Optional. Efficiency threshold. If the number of points exceeds nlarge, then only the border correction will be computed (by default), using a fast algorithm.
#' @param domain Optional. Calculations will be restricted to this subset of the window. See Details.
#' @param var.approx	Logical. If TRUE, the approximate variance of Kest(r) under CSR will also be computed.
#' @param ratio	Logical. If TRUE, the numerator and denominator of each edge-corrected estimate will also be saved, for use in analysing replicated point patterns.
#'
#' @return Returns an \code{fv} object.  See documentation for \code{Kest} function in \code{spatstat} package.
#' @author Joshua French
#' @import spatstat
#' @export
#' @references Waller, L.A. and Gotway, C.A. (2005).  Applied Spatial Statistics for Public Health Data.  Hoboken, NJ: Wiley.  Kulldorff, M. (1997) A spatial scan statistic. Communications in Statistics -- Theory and Methods 26, 1481-1496.
#' @examples 
#' data(grave)
#' kd = kdest(grave)
#' plot(kd)

kdest = function(x, case = 2, r=NULL, breaks=NULL, correction=c("border", "isotropic", "Ripley", "translate"), nlarge=3000, domain=NULL, var.approx=FALSE, ratio=FALSE)
{
  cases = which(x$marks == levels(x$marks)[case])
  K_case = Kest(x[cases, ], r = r, breaks = breaks, correction = correction, nlarge = nlarge,
                  Domain = domain, var.approx = var.approx, ratio = ratio)
  K_control = Kest(x[-cases, ], r = r, breaks = breaks, correction = correction, nlarge = nlarge,
                  domain = domain, var.approx = var.approx, ratio = ratio)
  eval.fv(K_case - K_control)
}