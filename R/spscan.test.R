#' Spatial Scan Test
#'
#' \code{spscan.test} performs the spatial scan test of Kulldorf (1997) for
#' case/control point data.
#'
#' The test is performed using the random labeling hypothesis.  The windows are
#' circular and extend from the observed data locations.  The clusters returned
#' are non-overlapping, ordered from most significant to least significant.  The
#' first cluster is the most likely to be a cluster.  If no significant clusters
#' are found, then the most likely cluster is returned (along with a warning).
#'
#' Setting \code{cl} to a positive integer MAY speed up computations on
#' non-Windows computers.  However, parallelization does have overhead cost, and
#' there are cases where parallelization results in slower computations.
#'
#' @param x A \code{\link[spatstat.geom]{ppp}} object with marks for the case
#'   and control groups.
#' @param nsim The number of simulations from which to compute the p-value.  A
#'   non-negative integer.  Default is 499.
#' @param alpha The significance level to determine whether a cluster is
#'   signficant.  Default is 0.1.
#' @param maxd The radius of the largest possible cluster to consider.  Default
#'   is \code{NULL}, i.e., half the maximum intercentroid distance.
#' @inheritParams logrr
#' @inheritParams pbapply::pbapply
#' @inheritParams qnn.test
#'   
#' @return Returns a list of length two of class 
#'   \code{spscan}. The first element (clusters) is a list 
#'   containing the significant, non-overlapping clusters, 
#'   and has the the following components: 
#'   \item{coords}{The
#'   centroid of the significant clusters.} 
#'   \item{r}{The 
#'   radius of the window of the clusters.} 
#'   \item{pop}{The 
#'   total population in the cluser window.} 
#'   \item{cases}{The observed number of cases in the 
#'   cluster window.} 
#'   \item{expected}{The expected number of
#'   cases in the cluster window.} 
#'   \item{smr}{Standarized 
#'   mortaility ratio (observed/expected) in the cluster 
#'   window.} 
#'   \item{rr}{Relative risk in the cluster 
#'   window.} \item{propcases}{Proportion of cases in the 
#'   cluster window.} 
#'   \item{loglikrat}{The loglikelihood 
#'   ratio for the cluster window (i.e., the log of the test
#'   statistic).} 
#'   \item{pvalue}{The pvalue of the test 
#'   statistic associated with the cluster window.} 
#'   
#'   Various additional pieces of information are included for plotting,
#'   printing
#' @author Joshua French
#' @export
#' @references Kulldorff M., Nagarwalla N. (1995) Spatial
#'   disease clusters: Detection and Inference. Statistics
#'   in Medicine 14, 799-810.
#'   
#'   Kulldorff, M. (1997) A spatial scan statistic.
#'   Communications in Statistics -- Theory and Methods 26,
#'   1481-1496.
#'   
#'   Waller, L.A. and Gotway, C.A. (2005). Applied Spatial
#'   Statistics for Public Health Data. Hoboken, NJ: Wiley.
#' @examples 
#' data(grave)
#' # apply scan method
#' out = spscan.test(grave, case = "affected", nsim = 99)
#' # print scan object
#' out
#' print(out, extra = TRUE)
#' # summarize results
#' summary(out)
#' # plot results
#' plot(out, chars = c(1, 20), main = "most likely cluster")
#' # extract clusters from out 
#' # each element of the list gives the location index of the events in each cluster
#' clusters(out)
#' # get warning if no significant cluster
#' out2 = spscan.test(grave, case = 2, alpha = 0.001, nsim = 0)
spscan.test <- 
  function(x, case = 2, nsim = 499, alpha = 0.1, 
            maxd = NULL, cl = NULL, longlat = FALSE) {
    x = arg_check_ppp_marks(x)
    case = arg_check_case(case, x)
    arg_check_nsim(nsim)

    idxcase = which(x$marks == levels(x$marks)[case])
    N = x$n
    N1 = length(idxcase)
    
    # get coordinates
    coords = matrix(c(x$x, x$y), ncol = 2)
    # compute distance matrix
    d = smerc::gedist(cbind(x$x, x$y), longlat = longlat)
    # set maxd if needed    
    if (is.null(maxd)) maxd = max(d)/2
    
    # compute nearest neighbor list
    mynn = nn(d, k = maxd, method = "d", self = TRUE)

    # determine the number of events inside the windows for successive
    # windows related to growing windows around each event location
    # to include the respective nearest neighbors stored in mynn
    Nin = unlist(lapply(mynn, function(x) seq_along(x)), use.names = FALSE)
    Nout = N - Nin
    # constant used in many places for calculation of log test statistic
    const = (N1 * log(N1) + (N - N1) * log(N - N1) - N * log(N))

    # number of nns for each observation
    nnn = unlist(lapply(mynn, length), use.names = FALSE)

    # factors related to number of neighbors
    # each event location possesses
    fac = rep(1:N, times = nnn)
    
    # calculate scan statistics for observed data
    z = numeric(N)
    z[idxcase] = 1

    tobs = 
      spscan.stat(
        N = N,
        N1 = N1,
        Nin = Nin,
        Nout = Nout,
        N1in = smerc::nn.cumsum(mynn, z), 
        const = const)
    
    # determine non-overlapping windows in decreasing
    # order of test statistic
    tobs_nn <- split(tobs, f = fac)
    noc_info <- smerc::noc_nn(mynn, tobs_nn)
    tobs <- noc_info$tobs
    
    if (nsim > 0) {
      tsim = spscan.sim(nsim = nsim, nnlist = mynn,
                        N = N, N1 = N1,
                        Nin = Nin, Nout = Nout,
                        const = const, cl = cl)
      pvalue = smerc::mc.pvalue(tobs, tsim)
    } else {
      pvalue = rep(1, length(tobs))
    }

    create_spscan(tobs = tobs,
                  pvalue = pvalue,
                  alpha = alpha, 
                  noc_info = noc_info,
                  coords = coords, z = z,
                  N = N, N1 = N1,
                  x = x, d = d, maxd = maxd,
                  longlat = longlat,
                  nsim = nsim)
}

#' Compute spatial scan statistic
#'
#' @param N Number of event locations
#' @param N1 Number of case events
#' @param Nin Number of event locations in each window
#' @param Nout Number of event locations outside each window
#' @param N1in Number of case events in each window
#' @param const Constant for computing test statistic
#'
#' @return A numeric vector of test statistics
#' @export
#' @keywords internal
spscan.stat <- function(N, N1, Nin, Nout, N1in, const) {
  if (missing(Nout)) {
    Nout = N - Nin
  }
  if (missing(const)) {
    const =  (N1 * log(N1) + (N - N1) * log(N - N1) - N * log(N))
  }
  N1out = N1 - N1in
  N0in = Nin - N1in
  N0out = Nout - N1out

  ### calculate scan statistics for observed data
  # calculate all test statistics
  tobsa = N1in * (log(N1in) - log(Nin))
  tobsa[which(is.nan(tobsa))] = 0
  tobsb = N1out * (log(N1out) - log(Nout))
  tobsb[which(is.nan(tobsb))] = 0
  tobsc = N0in * (log(N0in) - log(Nin))
  tobsc[which(is.nan(tobsc))] = 0
  tobsd = N0out * (log(N0out) - log(Nout))
  tobsd[which(is.nan(tobsd))] = 0
  tobs = tobsa + tobsb + tobsc + tobsd - const
  # correct test statistics for certain cases
  # incidence proportion in window is smaller than the 
  # proportion outside window
  tobs[N1in/Nin <= N1out/Nout] = 0
  return(tobs)
}

#' Compute spatial scan statistics for simulated data
#'
#' @inheritParams spscan.test
#' @inheritParams spscan.stat
#' @param nnlist A list of nearest neighbors from the
#'   \code{nn} function.
#' @return A vector of maximum test statistics
#' @export
#' @keywords internal
spscan.sim = function(nsim, nnlist, N, N1, Nin, Nout, const, cl) {
  # determine whether to parallelize results
  pbapply::pbvapply(seq_len(nsim), function(i) {
  
  # create vector of zeros with 1 in the case positions
  z = numeric(N)
  z[sample(1:N, N1)] = 1
  # for each element of the nn list,
  # cumulate the number of cases inside the successive window
  tall = 
    spscan.stat(
      N = N,
      N1 = N1,
      Nin = Nin,
      Nout = Nout,
      N1in = smerc::nn.cumsum(nnlist, z), 
      const = const)
  
  # return max of statistics for simulation
  return(max(tall))
  }, FUN.VALUE = numeric(1), USE.NAMES = FALSE, cl = cl)
}

#' Create spscan object
#'
#' @inheritParams spscan.test
#' @inheritParams spscan.stat
#' @param noc_info An object from the
#'   \code{\link[smerc]{noc_nn}} function.
#' @param z An indicator vector indicating which
#'   observations are cases.
#' @param tobs The vector of observed test statistics in
#'   descending order for non-overlapping windows.
#' @param pvalue The pvalues associated with \code{tobs}.
#' @param d A matrix of intercentroid distances for the
#'   event locations.
#'
#' @return An \code{spscan} object.
#' @export
#' @keywords internal
create_spscan = function(tobs, pvalue, alpha, noc_info,
                         coords, z,
                         N, N1, 
                         x, d, maxd,
                         longlat, 
                         nsim) {
  # determine which potential clusters are significant
  usigc = which(pvalue <= alpha, useNames = FALSE)
  
  # if there are no significant clusters, return most likely cluster
  if (length(usigc) == 0) {
    usigc = 1
    warning("No significant clusters.  Returning most likely cluster.")
  }
  
  # for the unique, non-overlapping clusters in order of significance,
  # find the associated test statistic, p-value, centroid,
  # window radius, cases in window, expected cases in window, 
  # population in window, standarized mortality ratio, relative risk,
  sig_tstat = tobs[usigc]
  sig_p = pvalue[usigc]
  sig_clusts = noc_info$clusts[usigc]
  start_region = unlist(lapply(sig_clusts, utils::head, n = 1),
                        use.names = FALSE)
  end_region = unlist(lapply(sig_clusts, utils::tail, n = 1),
                      use.names = FALSE)
  sig_coords = coords[start_region,, drop = FALSE]
  
  sig_r = d[cbind(start_region, end_region)]
  sig_popin = sapply(sig_clusts, length)
  sig_yin = smerc::zones.sum(sig_clusts, z)
  sig_ein = (N1/N)*sig_popin
  sig_smr = sig_yin/sig_ein
  sig_rr = (sig_yin/sig_popin)/((N1 - sig_yin)/(N - sig_popin))
  sig_prop_cases = sig_yin/sig_popin
  
  # reformat output for return
  clusters = vector("list", length(sig_clusts))
  for (i in seq_along(clusters)) {
    clusters[[i]]$locids = sig_clusts[[i]]
    clusters[[i]]$coords = sig_coords[i,, drop = FALSE]
    clusters[[i]]$r = sig_r[i]
    clusters[[i]]$pop = sig_popin[i]
    clusters[[i]]$cases = sig_yin[i]
    clusters[[i]]$expected = sig_ein[i]
    clusters[[i]]$smr = sig_smr[i]
    clusters[[i]]$rr = sig_rr[i]
    clusters[[i]]$propcases = sig_prop_cases[i]
    clusters[[i]]$loglikrat = sig_tstat[[i]]
    clusters[[i]]$pvalue = sig_p[i]
  }
  outlist = list(clusters = clusters, ppp = x,
                 maxd = maxd, method = "circular scan",
                 alpha = alpha,
                 longlat = longlat,
                 nsim = nsim,
                 total_cases = N1)
  class(outlist) = "spscan"
  return(outlist)
}