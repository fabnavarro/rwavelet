#' 2-d tensor wavelet transform (periodized, orthogonal).
#'
#' A two-dimensional Wavelet Transform is computed for the array x.
#' \code{qmf} filter may be obtained from \code{\link{MakeONFilter}}.
#' To reconstruct, use \code{\link{ITWT2_PO}}.
#'
#' @export FTWT2_PO
#' @param x 2-d image (n by n array, n dyadic).
#' @param L coarse level.
#' @param qmf quadrature mirror filter.
#' @return \code{wc} 2-d wavelet transform.
#' @examples
#' qmf <- MakeONFilter('Daubechies', 10)
#' L <- 0
#' x <- matrix(rnorm(2^2), ncol=2)
#' wc <- FTWT2_PO(x, L, qmf)
#' @seealso \code{\link{ITWT2_PO}}, \code{\link{MakeONFilter}}.

FTWT2_PO <- function(x, L, qmf) {
  q <- quadlength(x)
  n <- q$x
  J <- q$y
  # for (r in 1:n) { row <- x[r, ] wrow <- FWT_PO(row, L, qmf) x[r, ] <- wrow }
  # for (c in 1:n) { col <- x[, c] wcol <- FWT_PO(col, L, qmf) x[, c] <- wcol }
  # wc <- x
  x <- t(apply(x, 1, FUN = FWT_PO, L = L, qmf = qmf))
  wc <- apply(x, 2, FUN = FWT_PO, L = L, qmf = qmf)
  return(wc)
}

# Copyright (c) 1993. David L. Donoho

# Part of Wavelab Version 850 Built Tue Jan 3 13:20:40 EST 2006 This is
# Copyrighted Material For Copying permissions see COPYING.m Comments? e-mail
# wavelab@stat.stanford.edu
