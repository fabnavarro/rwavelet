#' Invert 2-d Translation Invariant Wavelet Transform
#'
#' @export IWT2_TI
#' @param tiwt translation-invariant wavelet transform table, (3(J-L)+1)n by n.
#' @param L degree of coarsest scale.
#' @param qmf orthonormal quadrature mirror filter.
#' @return \code{x} 2-d image reconstructed from translation-invariant transform TIWT.
#' @examples
#' x <- matrix(rnorm(2^2), ncol=2)
#' L <- 0
#' qmf <- MakeONFilter('Haar')
#' TIWT <- FWT2_TI(x, L, qmf)
#' xr <- IWT2_TI(TIWT,L,qmf)

IWT2_TI <- function(tiwt, L, qmf) {
  n <- dim(tiwt)[2]
  J <- log2(n)
  D <- J - L
  lastx <- (3 * D * n + 1):(3 * D * n + n)
  lasty <- 1:n
  x <- tiwt[lastx, lasty]
  #
  for (d in (D - 1):0) {
    l <- J - d - 1
    ns <- 2^(J - d)
    for (b1 in 0:(2^d - 1)) {
      for (b2 in 0:(2^d - 1)) {
        index10 <- packet(d + 1, 2 * b1, n)
        index20 <- packet(d + 1, 2 * b2, n)
        index11 <- packet(d + 1, 2 * b1 + 1, n)
        index21 <- packet(d + 1, 2 * b2 + 1, n)
        #
        wc00 <- rbind(cbind(x[index10, index20], tiwt[3 * d * n + index10,
          index20]), cbind(tiwt[(3 * d + 1) * n + index10, index20],
          tiwt[(3 * d + 2) * n + index10, index20]))
        wc01 <- rbind(cbind(x[index11, index20], tiwt[3 * d * n + index11,
          index20]), cbind(tiwt[(3 * d + 1) * n + index11, index20],
          tiwt[(3 * d + 2) * n + index11, index20]))
        wc10 <- rbind(cbind(x[index10, index21], tiwt[3 * d * n + index10,
          index21]), cbind(tiwt[(3 * d + 1) * n + index10, index21],
          tiwt[(3 * d + 2) * n + index10, index21]))
        wc11 <- rbind(cbind(x[index11, index21], tiwt[3 * d * n + index11,
          index21]), cbind(tiwt[(3 * d + 1) * n + index11, index21],
          tiwt[(3 * d + 2) * n + index11, index21]))
        x[packet(d, b1, n), packet(d, b2, n)] <- (IWT2_PO(wc00, l, qmf) +
          CircularShift(IWT2_PO(wc01, l, qmf), 0, -1) + CircularShift(IWT2_PO(wc10,
          l, qmf), -1, 0) + CircularShift(IWT2_PO(wc11, l, qmf), -1,
          -1))/4
      }
    }
  }
  return(x)
}

# Copyright (c) 1995. David L. Donoho and Thomas P.Y. Yu

# Part of Wavelab Version 850 Built Tue Jan 3 13:20:40 EST 2006 This is
# Copyrighted Material For Copying permissions see COPYING.m Comments? e-mail
# wavelab@stat.stanford.edu
