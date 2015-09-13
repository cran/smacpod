#' Spatial Scan Test
#' 
#' \code{spscan.test} performs the spatial scan test of Kulldorf (1997).
#' 
#' The test is performed using the random labeling hypothesis.  The windows are circular and extend from the observed data locations.  The clusters returned are non-overlapping, ordered from most significant to least significant.  The first cluster is the most likely to be a cluster.  If no significant clusters are found, then the most likely is returned (along with a warning).
#' 
#' @param x A \code{ppp} object from the \code{spatstat} package with marks for the case and control groups.
#' @param case The position of the name of the "case" group in levels(x$marks).  The default is 2.
#' @param nsim The number of simulations from which to compute p-value.
#' @param alpha The significance level to determine whether a cluster is signficiant.
#' @param nreport The frequency with which to report simulation progress.  The default is \code{nsim+ 1}, meaning no progress will be displayed.
#' @param maxd The radius of the largest possible cluster to consider.
#'
#' @return Returns a list with the following components: 
#' \item{coords}{The centroids of the significant clusters.}
#' \item{r}{The radii of the window of the significant clusters.}
#' \item{p}{The vector of p-values associated with the significant clusters.}
#' \item{ppp}{The original ppp object for which the scan test was performed.}
#' @author Joshua French
#' @import spatstat
#' @importFrom SpatialTools dist1
#' @export
#' @references Waller, L.A. and Gotway, C.A. (2005).  Applied Spatial Statistics for Public Health Data.  Hoboken, NJ: Wiley.  Kulldorff, M. (1997) A spatial scan statistic. Communications in Statistics -- Theory and Methods 26, 1481-1496.
#' @examples 
#' data(grave)
#' out = spscan.test(grave)
#' plot(out, main = "")
#' # get warning if no significant cluster
#' out2 = spscan.test(grave, alpha = 0.01)

spscan.test <- 
  function (x, case = 2, nsim = 499, alpha = 0.1, nreport = nsim + 
              1, maxd = NULL) 
  {
    if(!is.element("ppp", class(x))) stop("x must be a ppp object")
    if(is.null(x$marks)) stop("x must be marked as cases or controls")
    if(!is.factor(x$marks)) stop("The marks(x) must be a factor")
    nlev = length(levels(x$marks))
    if(case < 1 || case > nlev) stop("case must be an integer between 1 and length(levels(x$marks))")
    if(nsim < 0 | !is.finite(nsim)) stop("nsim must be a non-negative integer")
    
    idxcase = which(x$marks == levels(x$marks)[case])
    N = x$n
    N1 = length(idxcase)
    coords = matrix(c(x$x, x$y), ncol = 2)
    d = SpatialTools::dist1(coords)
    
    if(is.null(maxd)) maxd = max(d)/2
    
    mynn = nn(d, k = maxd, method = "d", self = TRUE)
    
    if (nreport <= nsim) 
      cat(paste("sims completed: "))
    
    # determine the number of events inside the windows for successive
    # windows related to growing windows around each event location
    # to include the respective nearest neighbors stored in mynn
    Nin = unlist(lapply(mynn, function(x) 1:length(x)), use.names = FALSE)
    Nout = N - Nin
    Tsim = numeric(nsim)
    for(i in 1:nsim)
    {
      # create vector of zeros with 1 in the case positions
      z = numeric(N)
      z[sample(1:N, N1)] = 1
      # for each element of the nn list,
      # cumulate the number of cases inside the successive window
      N1in = unlist(lapply(mynn, function(x) cumsum(z[x])), use.names = FALSE)
      N1out = N1 - N1in
      Tsim[i] = max((N1in/Nin)^N1in * 
                      (N1out/Nout)^(N1out) * 
                      (N1in/Nin > N1out/Nout), na.rm = TRUE)
      if ((i%%nreport) == 0) cat(paste(i, ""))
    }
    
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
    
    # observed test statistics, split by observation in order
    # of distance from observation centroid
    Tobs = split((N1in/Nin)^N1in*(N1out/Nout)^(N1out)*(N1in/Nin > N1out/Nout), fac)
    
    # position of most likely cluster centered at each observation
    Tmax_pos_each = lapply(Tobs, which.max)
    # value of statistic for most likely cluster centered at each observatoin
    Tmax_each = lapply(Tobs, max, na.rm = TRUE)
    # max statistic across all clusters
    Tscan = max(unlist(Tmax_each, use.names = FALSE), na.rm = TRUE)
    # Monte Carlo p-values for statistics of most likely cluster for each observation
    pvalue_each = lapply(Tmax_each, function(x) (sum(Tsim >= x) + 1)/(nsim + 1))
    
    # determine which potential clusters significant
    sig_clusters = which(pvalue_each <= alpha, useNames = FALSE)
    
    # if there are no significant clusters, return most likely cluster
    if(length(sig_clusters) == 0)
    {
      sig_clusters = which.max(Tmax_each)
      warning("No significant clusters.  Returning most likely cluster.")
    }
    
    # if there are no significant clusters, return most likely cluster
    if(length(sig_clusters) == 0)
    {
      sig_clusters = which.max(Tmax_each)
      print("No significant clusters.  Returning most likely cluster.")
    }
    
    # sort the distances for the significant clusters
    sig_sorted_d = apply(d[sig_clusters, , drop = FALSE], 1, sort)
    # determine index of sorted distance that corresponds to the radius
    # of the significant clusters 
    # note that rows and columns have been switched in sig_sorted_d
    sig_r_idx = cbind(unlist(Tmax_pos_each, use.names = FALSE)[sig_clusters], 1:length(sig_clusters))
    
    # pvalues of significant clsuters, ordered by significance
    sig_p = unlist(pvalue_each, use.names = FALSE)[sig_clusters]
    # order of significance
    o_sig = order(sig_p)
    sig_p = sig_p[o_sig]
    # radii of signifant clusters, ordered by significance
    sig_r = (sig_sorted_d[sig_r_idx])[o_sig]
    # centroids of signifcant clusters, ordered by significance
    sig_coords = (coords[sig_clusters,, drop = FALSE])[o_sig,, drop = FALSE]
    
    # determine the windows which intersect
    ci = circles.intersect(sig_coords, sig_r)
    # determine the non-overlapping circles, in order of priority
    noc = unique_circles(ci)
    
    out = list(coords = sig_coords[noc, ,drop = FALSE],
               r = sig_r[noc],
               p = sig_p[noc], 
               ppp = x)
    class(out) = "scan"
    return(out)
  }

## a little function to determine only return non-overlapping clusters,
unique_circles = function(x)
{
  remain = 1:ncol(x)
  i = 1
  u = 1
  while(i < ncol(x))
  {
    remain = setdiff(remain, which(x[i,]))
    if(length(remain) > 0)
    {
      i = min(remain)
      u = c(u, i)
    }else
    {
      i = ncol(x) + 1
    }
  }
  return(u)
}
