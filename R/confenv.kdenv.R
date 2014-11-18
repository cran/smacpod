#' Confidence envelopes for difference in estimated K functions
#' 
#' \code{confenv} determines confidence envelopes for the difference in estimated K functions for a set of cases and controls using a \code{kdenv} object.
#' 
#' @param object A \code{kdenv} object from the \code{kd.env} function.
#' @param level Confidence level.  Should be a number between 0 and 1.
#'
#' @return Returns an \code{confenvkd} object.  This is just a data frame with \code{r}, the distances at which the confidence envelope is calculated, \code{lo}, the lower bounds of the confidence envelopes, and \code{hi}, the upper bounds of the confidence envelopes.
#' @author Joshua French
#' @import spatstat
#' @export
#' @references Waller, L.A. and Gotway, C.A. (2005).  Applied Spatial Statistics for Public Health Data.  Hoboken, NJ: Wiley.
#' @examples 
#' data(grave)
#' kd = kdest(grave, nsim = 9)
#' ci = confenv(kd, level = 0.9)
#' plot(kd, legend = FALSE, main = "")
#' lines(ci, lty = 2)
#' legend("topleft", legend = c("observed", "average", "max/min", "90% conf. bands"), 
#' lty = c(1, 2, 1, 3), col = c("black", "red", "grey", "black"))
#' 
#' @rdname confenv
#' @export
confenv = function(object, level){
  UseMethod("confenv")
}

#' @rdname confenv
#' @export
confenv.kdenv = function(object, level = 0.95)
{
  simfuns <- as.data.frame(attr(object, "simfuns"))
  simfuns[,1] <- object$obs
  l = apply(simfuns, 1, quantile, prob  = (1 - level)/2)
  u = apply(simfuns, 1, quantile, prob = 1 - (1-level)/2)
  out = data.frame(r = object$r, lo = l, hi = u)
  class(out) = "confenvkd"
  return(out)
}

# kd = Kdest(grave)
# plot(kd)
# 
# myenv = Kdenv(grave, nsim = 99, r = seq(0, 2000, len = 513))
# plot(myenv, legend = FALSE, xlim = c(0, 2000))
# cb = confint.kdenv(myenv)
# lines(cb$r, cb$hi, lty = 3)
# lines(cb$r, cb$lo, lty = 3)
# legend("topleft", legend = c("observed", "average", "max/min", "95% conf. bands"), 
#  lty = c(1, 2, 1, 3), col = c("black", "red", "grey", "black"))

