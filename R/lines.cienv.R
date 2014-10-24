#' Adds lines to plot of \code{fv} object.
#' 
#' \code{lines} plots the confidence envelopes on a plot of the results of the \code{kdest} or \code{kdenv} objects.
#' 
#' @param x A \code{cienv} object from the \code{confint.kdenv} function.
#' @param ... Further graphical arguments for lines function.
#' 
#' @return A line is added to the plot on the current graphics device. No values are returned.
#' @author Joshua French
#' @export
#' @examples 
#' data(grave)
#' env = kd.env(grave, nsim = 19)
#' ci = confenv(env, level = .9)
#' plot(env, legend = FALSE)
#' lines(ci)
#' legend("topleft", legend = c("observed", "average", "max/min", "95% conf. bands"), 
#' lty = c(1, 2, 1, 3), col = c("black", "red", "grey", "black"))
#'
#' @rdname lines
#' @export
lines.confenvkd = function (x, ...) 
{
  lines(x$r, x$hi, ...)
  lines(x$r, x$lo, ...)
}

