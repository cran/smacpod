#' Log ratio of spatial densities
#' 
#' \code{logrr} computes the log ratio of spatial density functions.  This ratio is within an additive constant of the log relative risk.  The numerator in this ratio is related to the "cases" and the denominator to the "controls".
#' 
#' @param f An object from the \code{spdensity} function.  Typically is related to the "cases".
#' @param g An object from the \code{spdensity} function.  Typically is related to the "controls".
#'
#' @return The function produces an object of the same type as the \code{density.ppp} function from the \code{spatstat} package, with the intensity values replaces by the log ratio of the spatial densities f and g.  
#' @author Joshua French
#' @import spatstat
#' @export
#' @references Waller, L.A. and Gotway, C.A. (2005).  Applied Spatial Statistics for Public Health Data.  Hoboken, NJ: Wiley.  Kelsall, Julia E., and Peter J. Diggle. "Kernel estimation of relative risk." Bernoulli (1995): 3-16.  Kelsall, Julia E., and Peter J. Diggle. "Non-parametric estimation of spatial variation in relative risk." Statistics in Medicine 14.21-22 (1995): 2335-2342.
#' @examples 
#' data(grave)
#' cases = which(grave$marks == "affected")
#' f = spdensity(grave[cases,], sigma = 700)
#' g = spdensity(grave[-cases,], sigma = 700)
#' contour(logrr(f, g))

logrr = function(f, g)
{
  if(!all.equal(class(f), class(g)) || (max(class(f) == "spdensity") < 1))
  { stop("f and g should be objects from the spdensity function") }
  if(f$xcol!=g$xcol || f$yrow!=g$yrow) stop("f and g must use the same grid")
  f$v <- log(f$v/g$v)
  return(f)
}

