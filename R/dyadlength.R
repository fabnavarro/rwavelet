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
    warning("n != 2^J")
  }
  return(list(x = n, y = J))
}

# Part of Wavelab Version 850 Built Tue Jan 3 13:20:40 EST 2006 This is
# Copyrighted Material For Copying permissions see COPYING.m Comments? e-mail
# wavelab@stat.stanford.edu
