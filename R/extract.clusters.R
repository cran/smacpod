#' Extract clusters
#'
#' @param x An object of class \code{spscan} from the \code{\link{spscan.test}}
#'
#' @return A list. Each element of the list is a vector with the indices of event
#' locations in the associated cluster.
#' @export
#' @examples
#' data(grave)
#' # apply scan method
#' out = spscan.test(grave, nsim = 99)
#' # print scan object
#' clusters(out)
clusters = function(x) {
  if (!is.element("spscan", class(x))) {
    stop("x must have spscan class")
  }
  smerc::lget(x$clusters, "locids")
}