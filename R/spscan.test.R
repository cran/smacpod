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
#'   \code{scan}. The first element (clusters) is a list 
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
#' out2 = spscan.test(grave, case = 2, alpha = 0.001, nsim = 99)
spscan.test <- 
  function(x, case = 2, nsim = 499, alpha = 0.1, 
            maxd = NULL, cl = NULL, longlat = FALSE) {
    x = arg_check_ppp_marks(x)
    case = arg_check_case(case, x)
    arg_check_nsim(nsim)

    idxcase = which(x$marks == levels(x$marks)[case])
    N = x$n
    N1 = length(idxcase)
    coords = matrix(c(x$x, x$y), ncol = 2)

    d = smerc::gedist(cbind(x$x, x$y), longlat = longlat)
    
    if (is.null(maxd)) maxd = max(d)/2
    
    mynn = nn(d, k = maxd, method = "d", self = TRUE)
    
    # determine the number of events inside the windows for successive
    # windows related to growing windows around each event location
    # to include the respective nearest neighbors stored in mynn
    Nin = unlist(lapply(mynn, function(x) seq_along(x)), use.names = FALSE)
    Nout = N - Nin
    # constant used in many places on calculation of log test statistic
    const = (N1 * log(N1) + (N - N1) * log(N - N1) - N * log(N))
    # determine whether to parallelize results
    fcall = pbapply::pblapply

    # set up list of arguments to lapply or mclapply
    # X is the index (i), needed for counting
    # FUN determines the maximum spatial scan statistic for that data set
    fcall_list = list(X = as.list(1:nsim), 
                  FUN = function(i) {
                    # create vector of zeros with 1 in the case positions
                    z = numeric(N)
                    z[sample(1:N, N1)] = 1
                    # for each element of the nn list,
                    # cumulate the number of cases inside the successive window
                    N1in = unlist(lapply(mynn, function(x) cumsum(z[x])), use.names = FALSE)
                    N1out = N1 - N1in
                    N0in = Nin - N1in
                    N0out = Nout - N1out
                    
                    # calculate all test statistics
                    # correct test statistics for certain cases, specifically when
                    # number of cases in/out window is zero, number of controls
                    # in/out window is zero, rate in window less than outside window
                    talla = N1in * (log(N1in) - log(Nin))
                    talla[which(is.nan(talla))] = 0
                    tallb = N1out * (log(N1out) - log(Nout))
                    tallb[which(is.nan(tallb))] = 0
                    tallc = N0in * (log(N0in) - log(Nin))
                    tallc[which(is.nan(tallc))] = 0
                    talld = N0out * (log(N0out) - log(Nout)) 
                    talld[which(is.nan(talld))] = 0
                    tall = talla + tallb + tallc + talld - const
                    tall[N1in/Nin <= N1out/Nout] = 0

                    # return max of statistics for simulation
                    return(max(tall))
                  }, 
                  cl = cl)
    
    # mclapply or lapply to the simulated data sets the FUN in fcall_list
    # returns max scan statistic for that simulation
    # tsim = unlist(do.call(fcall, fcall_list), use.names = TRUE)
    tsim = unlist(do.call(fcall, fcall_list), use.names = TRUE)
    
    # number of nns for each observation
    nnn = unlist(lapply(mynn, length), use.names = FALSE)

    # factors related to number of neighbors
    # each event location possesses
    fac = rep(1:N, times = nnn)
    
    # calculate scan statistics for observed data
    z = numeric(N)
    z[idxcase] = 1
    N1in = unlist(lapply(mynn, function(x) cumsum(z[x])))
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

    # max scan statistic over all windows
    tscan = max(tobs)
    # observed test statistics, split by centroid in order 
    # of successive windows
    tobs_split = split(tobs, fac)
    
    # position of most likely cluster centered at each centroid
    tmax_pos = lapply(tobs_split, which.max)
    
    # determine the farthest neighbor of each centroid for
    # which the maximum scan statistic occurs for that centroid
    max_nn = mapply(function(a, b) a[b], a = mynn, b = tmax_pos)
    # index of the the windows where the max statistic occurs for each centroid
    # in the vector of all scan statistics
    tmax_idx = cumsum(c(0, nnn[-N])) + unlist(tmax_pos)
    
    # value of statistic for most likely cluster centered at each centroid
    tmax = lapply(tobs_split, max, na.rm = TRUE)
    
    # p-values associated with these max statistics for each centroid
    pvalue = unname(sapply(tmax, function(x) (sum(tsim >= x) + 1)/(nsim + 1)))
    
    # determine which potential clusters are significant
    sigc = which(pvalue <= alpha, useNames = FALSE)
    
    # if there are no significant clusters, return most likely cluster
    if (length(sigc) == 0) {
      sigc = which.max(tmax)
      warning("No significant clusters.  Returning most likely cluster.")
    }
    
    # which statistics are significant
    sig_tscan = unlist(tmax, use.names = FALSE)[sigc]
    # order statistics from smallest to largest
    o_sig = order(sig_tscan, decreasing = TRUE)
    # idx of significant clusters in order of significance
    sigc = sigc[o_sig]
    
    # determine the location ids in each significant cluster
    sig_regions = mapply(function(a, b) mynn[[a]][1:b], a = sigc, b = tmax_pos[sigc], SIMPLIFY = FALSE) 
    # determine idx of unique non-overlapping clusters
    u = noc(sig_regions)
    # return non-overlapping clusters (in order of significance)
    sig_regions = sig_regions[u]
    # unique significant clusters (in order of significance)
    usigc = sigc[u]
    
    # for the unique, non-overlapping clusters in order of significance,
    # find the associated test statistic, p-value, centroid,
    # window radius, cases in window, expected cases in window, 
    # population in window, standarized mortality ration, relative risk,
    sig_tstat = tmax[usigc]
    sig_p = pvalue[usigc]
    sig_coords = coords[usigc,, drop = FALSE]
    # sig_r = diag(SpatialTools::dist2(sig_coords, coords[max_nn[usigc], , drop = FALSE]))
    sig_r = smerc::gedist(sig_coords, coords[max_nn[usigc], , drop = FALSE], longlat = longlat)#, diagonal = TRUE)
    sig_popin = (Nin[tmax_idx])[usigc]
    sig_yin = (N1in[tmax_idx])[usigc]
    sig_ein = (N1/N)*sig_popin
    sig_smr = sig_yin/sig_ein
    sig_rr = (sig_yin/sig_popin)/((N1 - sig_yin)/(N - sig_popin))
    sig_prop_cases = sig_yin/sig_popin
    
    # reformat output for return
    clusters = vector("list", length(u))
    for (i in seq_along(clusters)) {
      clusters[[i]]$locids = sig_regions[[i]]
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
