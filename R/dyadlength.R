#' Find length and dyadic length of array
#'
#' @export dyadlength
#' @param x array of length n = 2^J (hopefully).
#' @return \code{n} length(x).
#' @return \code{J} least power of two greater than n.
#' @examples
#' x <- MakeSignal('Ramp', 8)
#' dyadlength(x)
#' @seealso \code{\link{quadlength}}, \code{\link{dyad}}

dyadlength <- function(x) {
  if (is.null(dim(x))) {
    n <- length(x)
  } else {
    n <- max(dim(x))
  }
  J <- ceiling(log(n)/log(2))
  if (2^J != n) {
    print("Warning in dyadlength: n != 2^J")
  }
  return(list(x = n, y = J))
}
