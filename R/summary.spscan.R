#' Summarize object from \code{\link{spscan.test}}.
#'
#' Summarize object of class \code{scan} from \code{\link{spscan.test}}.
#'
#' @param object An object of class \code{spscan}.
#' @param ... Additional arguments affecting the summary produced.
#' @param idx An index vector indicating the elements of \code{object$clusters}
#'   to print information for. The default is all clusters.
#' @param digits Integer indicating the number of decimal places.
#' @method summary spscan
#'
#' @return Returns/prints a data frame. Each row of the data frame summarizes
#'   the centroid of each cluster, the cluster radius, the number of events in
#'   the cluster, the number of cases in the cluster, the expected number of
#'   cases in the cluster, the relative risk of the cluster (cases/events in
#'   cluster)(cases/events outside cluster), the natural logarithm of the test
#'   statistic, and the associated p-value.
#'
#' @param object An \code{spscan} object.
#' @param ... Additional arguments affecting the summary produced.
#' @export
#' @examples
#' data(grave)
#' out = spscan.test(grave, nsim = 99, alpha = 0.8)
#' summary(out)
summary.spscan = function(object, ...,
                          idx = seq_along(object$clusters),
                          digits = 1) {
  if (min(idx) < 1 | max(idx) > length(object$clusters)) {
    stop("invalid idx values")
  }
  # get x and y position of centroids
  centroids = smerc::lget(object$clusters, "coords")
  centroid_x = sapply(centroids, function(x) x[1, 1])
  centroid_y = sapply(centroids, function(x) x[1, 2])

  # get cluster radius
  radius = base::round(smerc::sget(object$clusters, "r"), digits = digits)
  # number of events in cluster
  events = smerc::sget(object$clusters[idx], "pop")
  # number of cases in cluster
  cases = smerc::sget(object$clusters[idx], "cases")
  # expected number of cases in cluster
  ex = base::round(smerc::sget(object$clusters[idx], "expected"),
                   digits = digits)
  # relative risk
  rr = base::round(smerc::sget(object$clusters[idx], "rr"),
                   digits = digits)
  stat = base::round(smerc::sget(object$clusters[idx], "loglikrat"),
                     digits = digits)
  p = base::round(smerc::sget(object$clusters[idx], "pvalue"),
                     digits = 3)
  data.frame(centroid_x,
             centroid_y,
             radius,
             events,
             cases,
             ex,
             rr,
             stat,
             p,
             row.names = NULL
  )
}