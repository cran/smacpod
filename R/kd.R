#' Difference of estimated K functions
#' 
#' \code{kd} determines the difference in estimated K 
#' functions for a set of cases and controls.
#' 
#' This function relies internally on the 
#' \code{\link[spatstat.explore]{Kest}} and 
#' \code{\link[spatstat.explore]{eval.fv}}.  The arguments are essentially
#' the same as the \code{\link[spatstat.explore]{Kest}} function, 
#' and the user is referred there for more details about
#' the various arguments.
#' 
#' @param x A \code{\link[spatstat.geom]{ppp}} object with marks for the case
#'   and control groups.
#' @param domain Optional. Calculations will be restricted 
#'   to this subset of the window. See Details of 
#'   \code{\link[spatstat.explore]{Kest}}.
#' @inheritParams logrr
#' @inheritParams spatstat.explore::Kest
#'   
#' @return Returns an \code{fv} object.  See documentation 
#'   for \code{\link[spatstat.explore]{Kest}}.
#' @author Joshua French
#' @seealso \code{\link[spatstat.explore]{Kest}}, 
#'   \code{\link[spatstat.explore]{eval.fv}}
#' @references Waller, L.A. and Gotway, C.A. (2005). Applied
#'   Spatial Statistics for Public Health Data. Hoboken, NJ:
#'   Wiley.
#' @export
#' @examples 
#' data(grave)
#' kd = kd(grave)
#' plot(kd)
kd = function(x, case = 2, r = NULL, rmax = NULL, 
              breaks = NULL, 
              correction = c("border", "isotropic", "Ripley", "translate"), 
              nlarge = 3000, domain = NULL, 
              var.approx = FALSE, ratio = FALSE) {
  # select case based on number or text
  case = suppressMessages(arg_check_case(case, x))
  # which observations are cases
  cases = which(x$marks == levels(x$marks)[case])
  K_case = spatstat.explore::Kest(x[cases, ], r = r, rmax = rmax, breaks = breaks, correction = correction, nlarge = nlarge,
                Domain = domain, var.approx = var.approx, ratio = ratio)
  K_control = spatstat.explore::Kest(x[-cases, ], r = r, rmax = rmax, breaks = breaks, correction = correction, nlarge = nlarge,
                   domain = domain, var.approx = var.approx, ratio = ratio)
  spatstat.explore::eval.fv(K_case - K_control)
}