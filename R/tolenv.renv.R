#' Tolerance envelopes for log ratio of spatial densities.
#' 
#' \code{tolenv} determines tolerance envelopes for the log ratio of spatial densities of cases and controls.  Specifically, the function identifies where the observed log ratio of spatial densities exceeds what is expected under the random labeling hypothesis.  Results can be easily plotted using the contour or image functions.
#' 
#' \code{direction="both"} identifies locations where the observed log ratio of spatial densities is below and above, respectively, the (1-level)/2 and 1 - (1-level)/2 quantiles of log ratios of spatial densities simulated under the random labeling hypothesis.  "upper" finds where the observed ratio exceeds the "level" quantile.  "lower" finds where the observed ratio exceeds the 1 - level quantile.
#' 
#' The \code{z} argument of the list returned has a -1 for locations where the observed log ratio of spatial densities is below the tolerance envelope, a 0 for locations within the tolerance envelope, and a 1 for locations where the log ratio of spatial densities exceeds the tolerance envelope.
#' 
#' @param object An \code{renv} object from the \code{logrr.env} function.
#' @param level Confidence level.  Should be a number between 0 and 1.  Default is 0.95.
#' @param direction Default is "both".  Can also be "upper" or "lower".  
#' 
#' @return Returns an \code{tolenvr} object.  This is just a list with components \code{x}, \code{y}, and \code{z} the can be used with the \code{image} or \code{contour} functions quite easily.
#' @author Joshua French
#' @import spatstat
#' @export
#' @references Waller, L.A. and Gotway, C.A. (2005).  Applied Spatial Statistics for Public Health Data.  Hoboken, NJ: Wiley.
#' @examples 
#' data(grave)
#' renv = logrr(grave, nsim = 9)
#' tol = tolenv(renv, level = 0.8)
#' image(tol, col = c("blue", "white", "orange"), asp = 1)
#' contour(renv, add = TRUE)
#' polygon(grave$window$bdry[[1]])
#' @rdname tolenv
#' @export
tolenv = function(object, level, direction){
  UseMethod("tolenv")
}

#' @rdname tolenv
#' @export
tolenv.renv = function(object, level = 0.95, direction = "both")
{
  if(level <= 0 | level >= 1) stop("level must be between 0 and 1.")
  if(!is.element(direction, c("both", "upper", "lower"))) stop("direction is not valid.")
  alpha = 1 - level
  
  if(direction == "both")
  {
    tol = apply(object$simr, c(1, 2), quantile, 
                prob = c(alpha/2, 1-alpha/2), na.rm = TRUE)
  }
  else if(direction == "lower")
  {
    tol = apply(object$simr, c(1, 2), quantile, 
                prob = c(1 - level, 1), na.rm = TRUE)
  }
  else
  {
    tol = apply(object$simr, c(1, 2), quantile, 
                prob = c(0, level), na.rm = TRUE)
  }
  above = (object$simr[,,1] > tol[2,,]) + 0
  below = -1*(object$simr[,,1] < tol[1,,])
  both = above + below
  tolenv = list(x = object$xcol, y = object$yrow, z = t(both))
  class(tolenv) = "tolenvr"
  return(tolenv)
}