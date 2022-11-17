# Non-rejection envelopes for log ratio of spatial
# densities.
#
# \code{nrenv} determines non-rejection envelopes for the
# log ratio of spatial densities of cases and controls.
# Specifically, the function identifies where the observed
# log ratio of spatial densities exceeds what is expected
# under the random labeling hypothesis.  Results can be
# easily plotted using the contour or image functions.
#
# \code{alternative ="two.sided"} identifies locations where
# the observed log ratio of spatial densities is below and
# above, respectively, the (1-level)/2 and 1 - (1-level)/2
# quantiles of log ratios of spatial densities simulated
# under the random labeling hypothesis.  "greater" finds
# where the observed ratio exceeds the "level" quantile.
# "lower" finds where the observed ratio exceeds the 1 -
# level quantile.
#
# The \code{z} argument of the \code{\link[spatstat.geom]{im}}
# returns has a -1 for locations where the observed log
# ratio of spatial densities is below the non-rejection
# envelope, a 0 for locations within the non-rejection envelope,
# and a 1 for locations where the log ratio of spatial
# densities exceeds the non-rejection envelope.
#
# @param object An \code{im} object from the \code{logrr}
#   function.
# @param level Confidence level.  Should be a number between
#   0 and 1.  Default is 0.95.
# @param alternative Default is "two.sided".  Can also be
#   "greater" or "lower".
# @param envelope The type of envelope to construct. Either "pixelwise" or "simultaneous".
# @param return_sims Whether additional information should be provided
#
# @return Returns a \code{link[spatstat.geom]{im}} object
#   representing a two-dimensional pixel image.
# @author Joshua French
# @references Waller, L.A. and Gotway, C.A. (2005).  Applied
#   Spatial Statistics for Public Health Data.  Hoboken, NJ:
#   Wiley.
nrenv = function(object,
                 level = 0.90,
                 alternative = "two.sided",
                 envelope = "pixelwise",
                 return_sims = FALSE) {
  alpha = 1 - level
  alternative = match.arg(alternative, choices = c("two.sided", "lower", "upper"))
  if(envelope == "pixelwise") {
    if (alternative == "two.sided")   {
      tol = apply(object$simr, c(1, 2), stats::quantile, 
                  prob = c(alpha/2, 1 - alpha/2), na.rm = TRUE)
    } else if (alternative == "lower") {
      tol = apply(object$simr, c(1, 2), stats::quantile, 
                  prob = c(1 - level, 1), na.rm = TRUE)
    } else {
      tol = apply(object$simr, c(1, 2), stats::quantile, 
                  prob = c(0, level), na.rm = TRUE)
    }
    # indicator matrix when logrr above/below tolerance threshold
    above = (object$simr[,,1] > tol[2,,]) + 0
    below = -1*(object$simr[,,1] < tol[1,,])
  } else {
    # scale simr to account for heterogeneous variances
    object$simr = apply(object$simr, 1:2, scale)
    
    # determine min and max across each simulated slice
    mins = apply(object$simr, 1, min, na.rm = TRUE)
    maxs = apply(object$simr, 1, max, na.rm = TRUE)
    
    # determine quantiles depending on alternative
    if (alternative == "two.sided") {
      lo = stats::quantile(mins, prob = alpha/2, na.rm = TRUE)
      hi = stats::quantile(maxs, prob = 1 - alpha/2, na.rm = TRUE)
    } else if (alternative == "lower") {
      lo = stats::quantile(mins, prob = alpha, na.rm = TRUE)
      hi = stats::quantile(maxs, prob = 1, na.rm = TRUE)
    } else {
      lo = stats::quantile(mins, prob = 0, na.rm = TRUE)
      hi = stats::quantile(maxs, prob = 1 - alpha, na.rm = TRUE)
    }
    
    # replicate quantiles for each pixel
    tol = array(data = rep(c(lo, hi), each = prod(dim(object$simr)[2:3])),
                dim = c(dim(object$simr)[2:3], 2))
    
    # indicator above hi
    above = (object$simr[1, , ] > tol[, , 2]) + 0
    # negative indicator below hi
    below = -1 * (object$simr[1, , ] < tol[, , 1])
  }
  # unite indicator
  both = above + below
  # return results
  if(return_sims) {
    return(
      list(im = spatstat.geom::im(mat = both,
                                  xcol = object$xcol,
                                  yrow = object$yrow),
           hi = tol[2,,],
           low = tol[1,,])
      )
  } else {
    spatstat.geom::im(mat = both,
                      xcol = object$xcol,
                      yrow = object$yrow)
  }
}