#' 1d wavelet Block Thresholding
#'
#' This function is used for thresholding coefficients by group (or block)
#' according to the hard or soft thresholding rule.
#'
#' @export BlockThresh
#' @param wc wavelet coefficients.
#' @param j0 coarsest decomposition scale.
#' @param hatsigma estimator of noise variance.
#' @param L Block size (n mod L must be 0).
#' @param qmf Orthonormal quadrature mirror filter.
#' @param thresh 'hard' or 'soft'.
#' @return \code{wcb} wavelet coefficient estimators.
#' @examples
#' n <- 64
#' x <- MakeSignal('Ramp', n)
#' sig <- 0.01
#' y <- x + rnorm(n, sd=sig)
#' j0 <- 1
#' qmf <- MakeONFilter('Daubechies',8)
#' wc <- FWT_PO(y, j0, qmf)
#' L <- 2
#' wcb <- BlockThresh(wc, j0, sig, L, qmf, "hard")
#' @seealso \code{\link{invblock_partition}}, \code{\link{invblock_partition}}.

BlockThresh <- function(wc, j0, hatsigma, L, qmf, thresh = "hard") {
  n <- length(wc)
  if (n%%L != 0) {
    warning("The rest of the n/L division must be integer")
  }
  wcb <- wc
  J <- log2(n)
  lamb <- 4.50524
  if (thresh == "hard") {
    for (j in (0:(J - 1 - j0))) {
      HH <- wc[(2^(J - j - 1) + 1):2^(J - j)]
      HH <- block_partition(HH, L)
      Hm <- apply(HH^2, 2, sum) > lamb * hatsigma^2 * L
      HH <- HH * repmat(Hm, L, 1)
      HH <- invblock_partition(HH, 2^(J - j - 1), L)
      wcb[(2^(J - j - 1) + 1):2^(J - j)] <- HH
    }
  }
  if (thresh == "soft") {
    for (j in (0:(J - 1 - j0))) {
      HH <- wc[(2^(J - j - 1) + 1):2^(J - j)]
      HH <- block_partition(HH, L)
      Hm <- pmax(1 - lamb * hatsigma^2 * L/apply(HH^2, 2, sum), 0)
      HH <- HH * repmat(Hm, L, 1)
      HH <- invblock_partition(HH, 2^(J - j - 1), L)
      wcb[(2^(J - j - 1) + 1):2^(J - j)] <- HH
    }
  }
  # else { print('Possible thresholding rules: hard or soft') } TODO: add other
  # thresh rules
  return(wcb)
}
