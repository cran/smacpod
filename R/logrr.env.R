#' Envelopes for log ratio of spatial densities
#' 
#' \code{logrr.env} computes envelopes for the log ratio of spatial density functions.  The numerator in this ratio is related to the "cases" and the denominator to the "controls".
#' 
#' @param x Point pattern (object of class "ppp").
#' @param case The position of the name of the "case" group in levels(x$marks).  The default is 2.
#' @param nsim The number of simulated data sets from which to construct the envelopes under the random labeling hypothesis.
#' @param sigma  Standard deviation of isotropic Gaussian smoothing kernel. Either a numerical value, or a function that computes an appropriate value of sigma.
#' @param weights	Optional weights to be attached to the points. A numeric vector, numeric matrix, or an expression.
#' @param ... Additional arguments passed to pixellate.ppp and as.mask to determine the pixel resolution, or passed to sigma if it is a function.
#' @param edge	Logical flag: if TRUE, apply edge correction.
#' @param varcov	Variance-covariance matrix of anisotropic Gaussian kernel. Incompatible with sigma.
#' @param at	String specifying whether to compute the intensity values at a grid of pixel locations (at="pixels") or only at the points of x (at="points").
#' @param leaveoneout	Logical value indicating whether to compute a leave-one-out estimator. Applicable only when at="points".
#' @param adjust	Optional. Adjustment factor for the smoothing parameter.
#' @param diggle	Logical. If TRUE, use Diggle's edge correction, which is more accurate but slower to compute than the correction described under Details.
#' @param nreport How frequently to report progress on the simulation.  Default is 50.
#' 
#' @return The function produces an object of type \code{logrrenv}.  It's components are similar to those returned by the \code{density.ppp} function from the \code{spatstat} package, with the intensity values replaces by the log ratio of spatial densities of f and g.  Includes an array \code{simr} of dimension c(nx, ny, nsim + 1), where nx and ny are the number of x and y grid points used to estimate the spatial density.  \code{simr[,,1]} is the log ratio of spatial densities for the observed data and the remaining \code{nsim} elements in the third dimension of the array are the log ratios of spatial densities from a new ppp simulated under the random labeling hypothesis.
#' @author Joshua French
#' @import spatstat
#' @export
#' @references Waller, L.A. and Gotway, C.A. (2005).  Applied Spatial Statistics for Public Health Data.  Hoboken, NJ: Wiley.  Kelsall, Julia E., and Peter J. Diggle. "Kernel estimation of relative risk." Bernoulli (1995): 3-16.  Kelsall, Julia E., and Peter J. Diggle. "Non-parametric estimation of spatial variation in relative risk." Statistics in Medicine 14.21-22 (1995): 2335-2342.
#' @examples 
#' data(grave)
#' renv = logrr.env(grave, nsim = 9)

logrr.env = function(x, case = 2, nsim = 499, sigma = NULL, ..., weights=NULL, edge=TRUE, varcov=NULL, at="pixels", leaveoneout=TRUE, adjust=1, diggle=FALSE, nreport = 50)
{
  cases = which(x$marks == levels(x$marks)[case])
  N1 = length(cases)
  f = spdensity(x = x[cases,], sigma = sigma, ..., weights = weights,
                  edge = edge, varcov = varcov, at = at, leaveoneout = leaveoneout,
                  adjust = adjust, diggle = diggle)
  g = spdensity(x = x[-cases,], sigma = sigma, ..., weights = weights,
                  edge = edge, varcov = varcov, at = at, leaveoneout = leaveoneout,
                  adjust = adjust, diggle = diggle)
  r = logrr(f, g)
  simr = array(0, dim = c(r$dim, nsim + 1))
  simr[,,1] = r$v
  if(nreport <= nsim) cat("Simulations completed: ")
  for(i in 1:nsim)
  {
    cases = sample(x$n, N1)
    fsim = spdensity(x = x[cases,], sigma = sigma, ..., weights = weights,
                    edge = edge, varcov = varcov, at = at, leaveoneout = leaveoneout,
                    adjust = adjust, diggle = diggle)
    gsim = spdensity(x = x[-cases,], sigma = sigma, ..., weights = weights,
                    edge = edge, varcov = varcov, at = at, leaveoneout = leaveoneout,
                    adjust = adjust, diggle = diggle)
    rsim = logrr(fsim, gsim)
    simr[,,i + 1] = rsim$v
    if((i %% nreport) == 0){ cat(paste(i,"")); flush.console() }
  }
  r$simr = simr
  r$window = x$window
  class(r) = c(class(r), "renv")
  return(r)
}


