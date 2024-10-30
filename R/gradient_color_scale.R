#' Create gradient color scale with midpoint
#'
#' @param minval The minimum value of the data to be colored
#' @param maxval The maximum value of the data to be colored
#' @param n The desired number of breaks (approximately)
#' @param low The color for the low values
#' @param mid The color used for the midpoint of the gradient
#' @param high The color used for the high values
#' @param midpoint The midpoint of the color scale
#' @param ... Arguments passed to \code{\link[grDevices:colorRamp]{grDevices::colorRamp}}
#'
#' @return A list with \code{col} and \code{breaks} components specifying the colors and breaks of the color scale.
#' @export
#' @references Based on code from https://stackoverflow.com/a/10986203/5931362
#' @examples
#' data(grave)
#' lr = logrr(grave)
#' grad = gradient.color.scale(min(lr$v, na.rm = TRUE), max(lr$v, na.rm = TRUE))
#' plot(lr, col = grad$col, breaks = grad$breaks)
gradient.color.scale = function(minval, maxval, n = 11,
                                low = "blue", mid = "white", high = "red",
                                midpoint = 0, ...) {
  arg_check_gradient_color_scale(minval, maxval, n, low, mid, high, midpoint)
  if (length(minval) != 1 | !is.numeric(minval) | !is.finite(minval)) {
    stop("minval must be a single, finite number")
  }
  if (length(minval) != 1 | !is.numeric(minval) | !is.finite(minval)) {
    stop("minval must be a single, finite number")
  }
  
  nhalf = round(n/2)
  # vector of colors for values below threshold
  col1 = grDevices::colorRampPalette(colors = c(low, mid), ...)(nhalf)    
  # vector of colors for values above threshold
  col2 = grDevices::colorRampPalette(colors = c(mid, high), ...)(nhalf)
  # combine colors
  col = c(col1, col2)
  ## In your example, this line sets the color for values between 49 and 51. 
  # rampcols[c(nHalf, nHalf+1)] = rgb(t(col2rgb("white")), maxColorValue=256) 
  
  # create breaks below and above midpoint
  b1 = seq(minval, midpoint, length.out = nhalf + 1)
  b2 = seq(midpoint, maxval, length.out = nhalf + 1)[-1]
  breaks = c(b1, b2)
  
  return(list(col = col, breaks = breaks))
}

arg_check_gradient_color_scale = function(minval, maxval, n, low, mid, high, midpoint) {
  if (length(minval) != 1 | !is.numeric(minval) | !is.finite(minval)) {
    stop("minval must be a single, finite number")
  }
  if (length(maxval) != 1 | !is.numeric(maxval) | !is.finite(maxval)) {
    stop("maxval must be a single, finite number")
  }
  if (length(midpoint) != 1 | !is.numeric(midpoint) | !is.finite(midpoint)) {
    stop("midpoint must be a single, finite number")
  }
  if (length(n) != 1 | !is.numeric(n) | !is.finite(n) | n < 3) {
    stop("n must be a single, finite number >= 3")
  }
  if (midpoint < minval) {
    stop("midpoint must be greater than minval")
  }
  if (midpoint > maxval) {
    stop("midpoint must be less than maxval")
  }
  if (length(low) != 1) {
    stop("low must be a single character string")
  }
  if (length(mid) != 1) {
    stop("mid must be a single character string")
  }
  if (length(high) != 1) {
    stop("high must be a single character string")
  }
  grDevices::col2rgb(low)
  grDevices::col2rgb(mid)
  grDevices::col2rgb(high)
}


