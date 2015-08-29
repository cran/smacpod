#' Plots object from kdest of class \code{kdenv}. 
#'
#' @param x An object of class kdenv to be plotted.
#' @param ... Additional graphical parameters passed to \code{plot.fv} function.
#' @param shadecol1 Shade color for max/min envelopes. 
#' @param shadecol2 Shade color for confidence envelopes.
#' @import spatstat
#' @importFrom graphics plot polygon
#' @method plot kdenv
#' @export
#' @examples
#' data(grave)
#' kd1 = kdest(grave, nsim = 19, level = 0.9)
#' plot(kd1)

plot.kdenv = function(x, ..., shadecol1 = "grey", shadecol2 = "lightblue")
{
  if(!is.element("kdenv", class(x))) stop("x should be an object from kdest function")
  # if there were no simulations
  if(length(x) == 1) spatstat::plot.fv(x[[1]], ...)
  if(length(x) > 1)
  {
    # create main plot
    spatstat::plot.fv(x[[1]], legend = FALSE, shadecol = shadecol1, main = "", ...)
    # shade confidence bands
    graphics::polygon(c(x$r, rev(x$r)), c(x$qhi, rev(x$qlo)), col = shadecol2, border = NA)
    # redraw parts of plot covered by confidence bands
    spatstat::plot.fv(x[[1]], legend = FALSE, shadecol = NA, add = TRUE, ...)
  }
}