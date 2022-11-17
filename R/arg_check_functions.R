#' Argument check ppp object
#'
#' @param x A ppp object with factor marks
#' @noRd
arg_check_ppp_marks = function(x) {
  if (!is.element("ppp", class(x))) stop("x must be a ppp object")
  if (is.null(x$marks)) stop("x must be marked to distinguish case and  control levels")
  if (!is.factor(x$marks)) {
    message("converting marks(x) to a factor")
    x$marks <- factor(x$marks)
  }
  if (!is.factor(x$marks)) stop("The marks(x) must be a factor")
  return(x)
}

#' Check case argument
#'
#' @param case An integer or character string
#' @param x A valid ppp object with marks
#' @noRd
arg_check_case = function(case, x) {
  nlev = nlevels(x$marks)
  if (length(case) != 1) {
    stop("case must be a single value")
  }
  if (is.numeric(case)) {
    if (case < 1 || case > nlev) stop("case must be an integer between 1 and length(levels(x$marks))")
  } 
  if (is.character(case)) {
    if (!is.element(case, levels(x$marks))) {
      stop("The case value is not found in levels(x$marks)")
    }
    case = which(case == levels(x$marks))
  }
  message(paste(levels(x$marks)[case], "has been selected as the case group"))
  return(case)
}

#' Argument check nsim
#'
#' @param nsim A single, non-negative integer
#' @noRd
arg_check_nsim = function(nsim) {
  if (length(nsim) != 1) {
    stop("nsim must be a single value")
  }
  if (!is.numeric(nsim)) {
    stop("nsim must be a numeric value")
  }
  if (nsim < 0 | !is.finite(nsim)) {
    stop("nsim must be a non-negative integer")
  }
}

#' Argument check level
#'
#' @param level A single numeric value between 0 and 1
#' @noRd
arg_check_level = function(level) {
  if (length(level) != 1) {
    stop("level must be a single value")
  }
  if (!is.numeric(level)) {
    stop("level must be a numeric value")
  }
  if (level <= 0 | level >= 1) {
    stop("level must be between 0 and 1")
  }
}

#' Argument check alternative
#'
#' @param alternative One of "lower", "greater", "two.sided"
arg_check_alternative = function(alternative) {
  if (length(alternative) != 1) {
    stop("alternative must be a single value")
  }  
  if (!is.character(alternative)) {
    stop("alternative must be a single value")
  }
  if (!is.element(alternative, c("two.sided", "greater", "lower"))) {
    stop("alternative is not valid")
  }
}

#' Argument check longlat
#' @param longlat A single logical value
#' @noRd
arg_check_longlat = function(longlat) {
  if (length(longlat) != 1) {
    stop("longlat should be a single logical value")
  }
  if (!is.logical(longlat)) {
    stop("longlat should be a single logical value")
  }
}

#' Argument check number of nearest neighbors
#' @param q The number of nearest neighbors
#' @noRd
arg_check_q = function(q) {
  if (!is.numeric(q)) {
    stop("q must be a numeric value")
  }
  if (min(q) < 1) {
    stop("q must be 1 or larger")
  }
}

#' Argument check envelope type
#' @param envelope The type of envelope.
#' @noRd
arg_check_envelope = function(envelope) {
  if (length(envelope) != 1) {
    stop("envelope must be a single type")
  }
  if (!is.character(envelope)) {
    stop("envelope must be a character string")
  }
  if (!is.element(envelope, c("pixelwise", "simultaneous"))) {
    stop("envelope must be either 'pixelwise' or 'simultaneous'")
  }
}

#' Argument check return_sims logical
#' @param return_sims A logical value.
#' @noRd
arg_check_return_sims = function(return_sims) {
  if (length(return_sims) != 1) {
    stop("return_sims must be a single logical value")
  }
  if (!is.logical(return_sims)) {
    stop("return_sims must be a logical value")
  }
}