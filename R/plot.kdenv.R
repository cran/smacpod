#' Plot a \code{kdenv} object.
#'
#' Plots an object from \code{\link[smacpod]{kdest}} of
#' class \code{kdenv}.
#'
#' The solid line indicates the observed difference in the K
#' functions for the cases and controls.  The dashed line
#' indicates the average difference in the K functions
#' produced from the data sets simulated under the random
#' labeling hypothesis when executing the \code{kdest}
#' function. The shaded areas indicate the tolerance envelopes
#' constructed in \code{x} for tolerance level \code{level} and
#' the min/max envelopes constructed under the random labeling
#' hypothesis.
#'
#' @param x An object of class \code{kdenv} produced by
#'   \code{\link[smacpod]{kdest}}.
#' @param ... Additional graphical parameters passed to the
#'   \code{\link[spatstat.explore]{plot.fv}} function, which is used
#'   internally for plotting.
#' @param shadecol1 Color for min/max tolerance envelopes generated under the random labeling hypothesis.
#'   The default is a dark grey.
#' @param shadecol2 Shade color for non-rejection envelopes.
#'   The default is \code{"lightgrey"}.
#' @param main A main title for the plot.  The default is blank.
#' @param legend Logical for whether a legend should
#'   automatically be displayed.  Default is \code{FALSE}.
#'   See Details for an explanation of the components of the
#'   plot.
#' @method plot kdenv
#' @seealso \code{\link[spatstat.explore]{plot.fv}}
#' @export
#' @examples
#' data(grave)
#' kdenv = kdest(grave, nsim = 19, level = 0.9)
#' plot(kdenv)
#' plot(kdenv, legend = TRUE)
plot.kdenv = function(x, ..., shadecol1 = "darkgrey", shadecol2 = "lightgrey", main = "", legend = FALSE) {
  if (!is.element("kdenv", class(x))) stop("x should be an object from kdest function")
  # if there were no simulations
  if (length(x) == 1) spatstat.explore::plot.fv(x[[1]], main = main, legend = legend, ...)
  if (length(x) > 1) {
    # create main plot
    spatstat.explore::plot.fv(x[[1]], main = main, legend = legend, shadecol = shadecol1, ...)
    # shade confidence bands
    # do some additional prep in case xlim is specified, otherwise
    # the polygon will go beyond the desired boundary
    plotargs = list(...)
    xmax = max(x$r, na.rm = TRUE)
    if (!is.null(plotargs$xlim)) { xmax = max(plotargs$xlim) }
    pin = which(x$r <= xmax)
    
    graphics::polygon(c(x$r[pin], rev(x$r[pin])), c(x$qhi[pin], rev(x$qlo[pin])), 
                      col = shadecol2, border = NA)
    # redraw parts of plot covered by confidence bands
    spatstat.explore::plot.fv(x[[1]], main = "", legend = FALSE, shadecol = NA, add = TRUE, ...)
  }
}