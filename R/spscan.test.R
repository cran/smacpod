#' Spatial Scan Test
#' 
#' \code{spscan.test} performs the spatial scan test of Kulldorf (1997).
#' 
#' The test is performed using the random labeling hypothesis.  The windows are circular and extend from the observed data locations.  The minimum window radius is the minimum distance between cases.  The default upper bound is half the maximum interevent distance.
#' 
#' @param x A \code{ppp} object from the \code{spatstat} package with marks for the case and control groups.
#' @param nsim The number of simulations from which to compute p-value.
#' @param case The position of the name of the "case" group in levels(x$marks).  The default is 2.
#' @param len The length of the vector of radii from which to perform the test.
#' @param nreport The frequency with which to report simulation progress.  The default is \code{nsim+ 1}, meaning no progress will be displayed.
#' @param maxr The max distance of the vector of radii from which to perform the test.
#'
#' 
#' @return Returns a list with the following components: 
#' \item{pvalue}{The p-value of the spatial scan test.}
#' \item{mlc}{The location of the most likely cluster.}
#' \item{r}{The radius of the window of the most likely cluster.}
#' @author Joshua French
#' @import spatstat
#' @export
#' @references Waller, L.A. and Gotway, C.A. (2005).  Applied Spatial Statistics for Public Health Data.  Hoboken, NJ: Wiley.  Kulldorff, M. (1997) A spatial scan statistic. Communications in Statistics -- Theory and Methods 26, 1481-1496.
#' @examples 
#' data(grave)
#' out = spscan.test(grave)
#' plot(grave)
#' # draw location of most likely cluster
#' # uses function from plotrix package
#' library(plotrix)
#' draw.circle(out$mlc[1], out$mlc[2], out$r)

spscan.test = function(x, nsim = 499, case = 2, len = 50, nreport = nsim + 1, maxr = NULL)
{
  
  if(!is.element("ppp", class(x))) stop("x must be a ppp object")
  if(is.null(x$marks)) stop("x must be marked as cases or controls")
  if(!is.factor(x$marks)) stop("The marks(x) must be a factor")
  nlev = length(levels(x$marks))
  if(case < 1 || case > nlev) stop("case must be an integer between 1 and length(levels(x$marks))")
  if(nsim < 0 | !is.finite(nsim)) stop("nsim must be a non-negative integer")
  
  casename = levels(x$marks)[case]
  idxcase = which(x$marks == casename)
  
  # number of observations
  N = x$n
  N1 = length(idxcase)
  # distances between observations
  coords = cbind(x$x, x$y)
  D = array(as.matrix(dist(coords)), dim = c(N, N)) # matrix without attributes
  pD = D[, idxcase]
  minD = min(pD[pD > 0])
  if(is.null(maxr))
  {
    maxD = max(D)/2
  }else
  {
    maxD = maxr
  }
  r = seq(minD, maxD, len = len)  
  
  # number of observations in each radius
  Nin = sapply(r, function(r) rowSums(D <= r))
  Nout = N - Nin # outside each radiou
  
  N1in = sapply(r, function(r) rowSums(pD <= r))
  N1out = N1 - N1in
  T = (N1in/Nin)^N1in*(N1out/Nout)^(N1out)*(N1in/Nin > N1out/Nout)*(Nin > 0)
  
  Tscan = max(T, na.rm = TRUE)
  maxpos = which(T == Tscan, arr.ind = TRUE)
  maxloc = coords[maxpos[1,1],]
  mlcr  = r[maxpos[1,2]]
  
  # display results if appropriate
  if(nreport <= nsim) cat(paste("sims completed: "))
  simTscan = numeric(nsim)
  for(i in 1:nsim)
  {
    newidxcase = sample(1:N, N1)
    pD = D[,newidxcase]
    N1in = sapply(r, function(r) rowSums(pD <= r))
    N1out = N1 - N1in
    T = (N1in/Nin)^N1in*(N1out/Nout)^(N1out)*(N1in/Nin > N1out/Nout)*(Nin > 0)
    simTscan[i] = max(T, na.rm = TRUE)
    
    if((i %% nreport) == 0) cat(paste(i,""))
  } 
  if(nreport <= nsim) cat(paste("\n"))
  
  pvalue = (sum(simTscan > Tscan) + 1)/(nsim + 1)
  print(paste("p-value =",pvalue))
  out = NULL
  out$mlc = maxloc
  out$r = mlcr
  out
}

