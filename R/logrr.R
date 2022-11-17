#' Log ratio of spatial densities
#'
#' \code{logrr} computes the estimated log relative risk of
#' cases relative to controls. The log relative risk at
#' location s is defined as \code{r(s) = ln(f(s)/g(s))}. The
#' numerator, \code{f(s)}, is the spatial density of the
#' case group. The denominator, \code{g(s)}, is the spatial
#' density of the control group. If \code{nsim > 0}, then
#' pointwise (at each pixel) tolerance envelopes are
#' estimated under the random labeling hypothesis. The
#' tolerance envelopes can be used to assess pixels where
#' the log relative risk differs significantly from zero.
#' See Details.
#'
#' If \code{nsim=0}, the \code{plot} function creates a heat
#' map of the log relative risk. If \code{nsim > 0}, the
#' \code{plot} function colors the pixels where the
#' estimated log relative risk is outside the tolerance
#' envelopes created under the random labeling hypothesis
#' (i.e., pixels with potential clustering of cases or
#' controls). Colored regions with values above 0 indicate a
#' cluster of cases relative to controls (without
#' controlling for multiple comparisons), i.e., a region
#' where the the density of the cases is greater than the
#' the density of the controls. Colored regions with values
#' below 0 indicate a cluster of controls relative to cases
#' (without controlling for multiple comparisons), i.e., a
#' region where the density of the controls is greater than
#' the density of the cases.
#'
#' The \code{two.sided} alternative test constructs
#' two-sided tolerance envelopes to assess whether the
#' estimated \code{r(s)} deviates more than what is expected
#' under the random labeling hypothesis.  The \code{greater}
#' alternative constructs an upper tolerance envelope to
#' assess whether the estimated \code{r(s)} is greater than
#' what is expected under the random labeling hypothesis,
#' i.e., where there is clustering of cases relative to
#' controls. The \code{lower} alternative constructs a lower
#' tolerance envelope to assess whether the estimated
#' \code{r(s)} is lower than what is expected under the
#' random labeling hypothesis, i.e., where there is
#' clustering of controls relative to cases.
#'
#' If the estimated density of the case or control group
#' becomes too small, this function may produce warnings due
#' to numerical underflow. Increasing the bandwidth
#' (\code{sigma}) may help.
#'
#' @param x A \code{\link[spatstat.geom]{ppp}} object
#'   package with marks for the case and control groups.
#'   \code{x$marks} is assumed to be a factor. Automatic
#'   conversion is attempted if it is not.
#' @inheritParams spatstat.explore::density.ppp
#' @param sigma Standard deviation of isotropic smoothing
#'   kernel for cases. Either a numerical value, or a
#'   function that computes an appropriate value of
#'   \code{sigma}. If not specified, then
#'   \code{\link[spatstat.explore]{bw.relrisk}} is used.
#' @param sigmacon Standard deviation of isotropic smoothing
#'   kernel for controls.  Default is the same as
#'   \code{sigma}.
#' @param case The name of the desired "case" group in
#'   \code{levels(x$marks)}. Alternatively, the position of
#'   the name of the "case" group in \code{levels(x$marks)}.
#'   Since we don't know the group names, the default is 2,
#'   the second position of \code{levels(x$marks)}.
#'   \code{x$marks} is assumed to be a factor.  Automatic
#'   conversion is attempted if it is not.
#' @param nsim The number of simulated data sets from which
#'   to construct tolerance envelopes under the random
#'   labeling hypothesis.  The default is 0 (i.e., no
#'   envelopes).
#' @param level The level of the tolerance envelopes.
#' @param alternative The type of envelopes to construct.
#'   The default is \code{"two.sided"} (upper and lower
#'   envelopes).  The values \code{"less"} (lower envelope)
#'   and \code{"greater"} (upper envelope) are also valid.
#' @param envelope The type of envelope to construct. The
#'   default is \code{"pixelwise"}. The other option is
#'   \code{"simulataneous"}, which controls the tolerance
#'   level across the entire study area.
#' @param bwargs A list of arguments for the bandwidth
#'   function supplied to \code{sigma} and \code{sigmacon},
#'   if applicable.
#' @param return_sims A logical value indicating whether
#'   parts of the simulated data shoudl be returned. The
#'   default is \code{FALSE}. This is mostly used for
#'   debugging purposes.
#'
#' @return The function produces an object of type
#'   \code{logrrenv}.  Its components are similar to those
#'   returned by the
#'   \code{\link[spatstat.explore]{density.ppp}}, with the
#'   intensity values replaced by the log ratio of spatial
#'   densities of f and g.  Includes an array \code{simr} of
#'   dimension c(nx, ny, nsim + 1), where nx and ny are the
#'   number of x and y grid points used to estimate the
#'   spatial density. \code{simr[,,1]} is the log ratio of
#'   spatial densities for the observed data, and the
#'   remaining \code{nsim} elements in the third dimension
#'   of the array are the log ratios of spatial densities
#'   from a new ppp simulated under the random labeling
#'   hypothesis.
#' @author Joshua French (and a small chunk by the authors
#'   of the \code{\link[spatstat.explore]{density.ppp}})
#'   function for consistency with the default behavior of
#'   that function).
#' @export
#' @references Waller, L.A. and Gotway, C.A. (2005). Applied
#'   Spatial Statistics for Public Health Data. Hoboken, NJ:
#'   Wiley.
#'
#'   Kelsall, Julia E., and Peter J. Diggle. "Kernel
#'   estimation of relative risk." Bernoulli (1995): 3-16.
#'
#'   Kelsall, Julia E., and Peter J. Diggle. "Non-parametric
#'   estimation of spatial variation in relative risk."
#'   Statistics in Medicine 14.21-22 (1995): 2335-2342.
#'   
#'   Hegg, Alex and French, Joshua P. (2022) "Simultaneous
#'   tolerance bands of log relative risk for 
#'   case-control point data. Technical report.
#' @examples
#' data(grave)
#' # estimate and plot log relative risk
#' r = logrr(grave, case = "affected")
#' plot(r)
#' # use scott's bandwidth
#' r2 = logrr(grave, case = 2, sigma = spatstat.explore::bw.scott)
#' plot(r2)
#' # construct pointwise tolerance envelopes for log relative risk
#' \dontrun{
#' renv = logrr(grave, nsim = 9)
#' print(renv) # print information about envelopes
#' plot(renv) # plot results
#' # plot using a better gradient
#' grad = gradient.color.scale(min(renv$v, na.rm = TRUE), max(renv$v, na.rm = TRUE))
#' plot(renv, col = grad$col, breaks = grad$breaks, conlist = list(col = "lightgrey"))}
logrr = function(x, sigma = NULL, sigmacon = NULL, case = 2, 
                 nsim = 0, level = 0.90, alternative = "two.sided", 
                 envelope = "pixelwise",
                 ..., 
                 bwargs = list(), weights = NULL, edge = TRUE, 
                 varcov = NULL, at = "pixels", leaveoneout = TRUE, 
                 adjust = 1, diggle = FALSE, 
                 kernel = "gaussian",
                 scalekernel = is.character(kernel),
                 positive = FALSE, verbose = TRUE, return_sims = FALSE) {
  # check argument validity
  x = arg_check_ppp_marks(x)
  case = arg_check_case(case, x)
  arg_check_nsim(nsim)
  arg_check_level(level)
  arg_check_alternative(alternative)
  arg_check_envelope(envelope)
  arg_check_return_sims(return_sims)
  
  alpha = 1 - level

  # determine bandwidth if necessary
  if (is.function(sigma)) { # use user-supplied function, if given
    which_bwargs <- which(names(bwargs) %in% names(formals(sigma))[-1])
    if (length(which_bwargs) > 0 ) {
      sigma = do.call(sigma, c(list(X = x, bwargs[which_bwargs])))
    } else {
      sigma = do.call(sigma, list(X = x))
    }
  }
  if (is.null(sigma)) { # use spatstat.explore::bw.relrisk if nothing given
    which_bwargs <- names(bwargs) %in% names(formals(spatstat.explore::bw.relrisk))[-1]
    if (length(which_bwargs) > 0 ) {
      sigma = do.call(spatstat.explore::bw.relrisk, c(list(X = x, bwargs[which_bwargs])))
    } else {
      sigma = do.call(spatstat.explore::bw.relrisk, list(X = x))
    }
  }
  if (is.null(sigmacon)) sigmacon = sigma # determine sigmacon, if NULL
  
  cases = which(x$marks == levels(x$marks)[case])
  N1 = length(cases)
  r = spdensity(x = x[cases,], sigma = sigma, ..., 
                weights = weights[cases],
                edge = edge, varcov = varcov, at = at, 
                leaveoneout = leaveoneout,
                adjust = adjust, diggle = diggle,
                kernel = kernel, 
                scalekernel = scalekernel,
                positive = positive, verbose = verbose)
  
  g = spdensity(x = x[-cases,], sigma = sigmacon, ..., 
                weights = weights[-cases],
                edge = edge, varcov = varcov, at = at, 
                leaveoneout = leaveoneout,
                adjust = adjust, diggle = diggle,
                kernel = kernel, 
                scalekernel = scalekernel,
                positive = positive, verbose = verbose)
  r$v = log(r$v) - log(g$v)
  r$nrenv = NULL
  
  if (nsim > 0) {
    simr2 <- logrr_sim(nsim = nsim, sigma = sigma, sigmacon = sigmacon,
                       x = x, N1 = N1, 
                       level = level, alternative = alternative,
                       envelope = envelope, ..., 
                       bwargs = bwargs, weights = weights,
                       edge = edge,
                       varcov = varcov, at = at, leaveoneout = leaveoneout,
                       adjust = adjust, diggle = diggle, kernel = kernel,
                       scalekernel = scalekernel, positive = positive,
                       verbose = verbose, return_sims = return_sims)
    r$simr = abind::abind(append(list(r$v), smerc::lget(simr2, "r")), along = 3)
    # only needed if return_sim is TRUE
    if (return_sims) {
      r$simr_cases = smerc::lget(simr2, "cases")
      r$simr_fsim = smerc::lget(simr2, "fsim")
      r$simr_gsim = smerc::lget(simr2, "gsim")
      r$temp_nrenv = nrenv(r,
                         level = level,
                         alternative = alternative,
                         envelope = envelope,
                         return_sims = return_sims)
      r$nrenv = r$temp_nrenv$im
    } else {
      r$nrenv = nrenv(r,
                      level = level,
                      alternative = alternative,
                      envelope = envelope,
                      return_sims = return_sims)
    }
    class(r) = c("logrrenv", class(r))
  }
  
  # add details
  r$window = x$window
  r$envelope = envelope
  r$case_label = levels(x$marks)[case]
  r$control_label = levels(x$marks)[-case]
  r$nsim = nsim
  r$level = level
  
  return(r)
}

# function that selects new cases based on the RLH
# estimates the spatial density of the cases and controls
# and returns the logrr (and other info if returnsims = TRUE)
logrr_sim = function(nsim, sigma, sigmacon, x, N1,  
                     level, alternative, 
                     envelope,
                     ..., 
                     bwargs, weights,
                     edge, 
                     varcov, at, leaveoneout, 
                     adjust, diggle, 
                     kernel,
                     scalekernel,
                     positive, verbose, return_sims) {
  out = pbapply::pblapply(seq_len(nsim), function(i) {
    cases = sample(x$n, N1)
    fsim = spdensity(x = x[cases,], sigma = sigma, ..., 
                     weights = weights[cases],
                     edge = edge, varcov = varcov, at = at, 
                     leaveoneout = leaveoneout,
                     adjust = adjust, diggle = diggle,
                     kernel = kernel, 
                     scalekernel = scalekernel,
                     positive = positive, verbose = verbose)
    
    gsim = spdensity(x = x[-cases,], sigma = sigmacon, ..., 
                     weights = weights[-cases],
                     edge = edge, varcov = varcov, at = at, 
                     leaveoneout = leaveoneout,
                     adjust = adjust, diggle = diggle,
                     kernel = kernel, 
                     scalekernel = scalekernel,
                     positive = positive, verbose = verbose)
    
    if(return_sims) {
      return(list(cases = cases,
                  fsim = fsim,
                  gsim = gsim,
                  r = log(fsim$v) - log(gsim$v)))
    } else {
      return(list(r = log(fsim$v) - log(gsim$v)))
    }
  })
}