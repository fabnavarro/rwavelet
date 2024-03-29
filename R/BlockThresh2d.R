#' 2d Wavelet Block thresholding
#'
#' This function is used to threshold the coefficients by group (or block).
#'
#' @param wc wavelet coefficients.
#' @param j0 coarsest decomposition scale.
#' @param hatsigma estimator of noise variance.
#' @param L Block size (n mod L must be 0).
#' @param lamb Threshold parameter.
#' @param qmf Orthonormal quadrature mirror filter.
#' @param thresh Thresholding rule: 'hard', or 'JamesStein'.
#' @return \code{wcb} Thresholded wavelet coefficients.

BlockThresh2d <- function(wc, j0, hatsigma, L = 4, qmf, lamb = 4.50524, thresh = "JamesStein") {
  nx <- dim(wc)[1]
  ny <- dim(wc)[2]
  if (nx != ny) {
    stop('Matrix must be square')
  }
  n <- nx
  if (n%%L != 0) {
    warning("The rest of the n/L division must be integer")
  }
  wcb <- wc
  J <- log2(n)
  # if (thresh == "soft") {
  #   for (j in (0:(J - 1 - j0))) {
  #     HH <- wc[(2^(J - j - 1) + 1):2^(J - j), (2^(J - j - 1) + 1):2^(J -j)]
  #     HH <- block_partition2d(HH, L)
  #     HHmask <- pmax(1 - sqrt(lamb) * hatsigma * sqrt(L)/apply(HH, 2, sum), 0)
  #     HH <- HH * repmat(HHmask, L * L, 1)
  #     HH <- invblock_partition2d(HH, 2^(J - j - 1), L)
  #     wcb[(2^(J - j - 1) + 1):2^(J - j), (2^(J - j - 1) + 1):2^(J - j)] <- HH
  #
  #     HL <- wc[(2^(J - j - 1) + 1):2^(J - j), (1:2^(J - j - 1))]
  #     HL <- block_partition2d(HL, L)
  #     HLmask <- pmax(1 - sqrt(lamb) * hatsigma * sqrt(L)/apply(HL, 2, sum), 0)
  #     HL <- HL * repmat(HLmask, L * L, 1)
  #     HL <- invblock_partition2d(HL, 2^(J - j - 1), L)
  #     wcb[(2^(J - j - 1) + 1):2^(J - j), 1:2^(J - j - 1)] <- HL
  #
  #     LH <- wc[1:2^(J - j - 1), (2^(J - j - 1) + 1):2^(J - j)]
  #     LH <- block_partition2d(LH, L)
  #     LHmask <- pmax(1 - sqrt(lamb) * hatsigma * sqrt(L)/apply(LH, 2, sum), 0)
  #     LH <- LH * repmat(LHmask, L * L, 1)
  #     LH <- invblock_partition2d(LH, 2^(J - j - 1), L)
  #     wcb[1:2^(J - j - 1), (2^(J - j - 1) + 1):2^(J - j)] <- LH
  #   }
  # }
  if (thresh == "JamesStein") {
    for (j in (0:(J - 1 - j0))) {
      HH <- wc[(2^(J - j - 1) + 1):2^(J - j), (2^(J - j - 1) + 1):2^(J -j)]
      HH <- block_partition2d(HH, L)
      HHmask <- pmax(1 - lamb * hatsigma^2 * L/apply(HH^2, 2, sum), 0)
      HH <- HH * repmat(HHmask, L * L, 1)
      HH <- invblock_partition2d(HH, 2^(J - j - 1), L)
      wcb[(2^(J - j - 1) + 1):2^(J - j), (2^(J - j - 1) + 1):2^(J - j)] <- HH

      HL <- wc[(2^(J - j - 1) + 1):2^(J - j), (1:2^(J - j - 1))]
      HL <- block_partition2d(HL, L)
      HLmask <- pmax(1 - lamb * hatsigma^2 * L/apply(HL^2, 2, sum), 0)
      HL <- HL * repmat(HLmask, L * L, 1)
      HL <- invblock_partition2d(HL, 2^(J - j - 1), L)
      wcb[(2^(J - j - 1) + 1):2^(J - j), 1:2^(J - j - 1)] <- HL

      LH <- wc[1:2^(J - j - 1), (2^(J - j - 1) + 1):2^(J - j)]
      LH <- block_partition2d(LH, L)
      LHmask <- pmax(1 - lamb * hatsigma^2 * L/apply(LH^2, 2, sum), 0)
      LH <- LH * repmat(LHmask, L * L, 1)
      LH <- invblock_partition2d(LH, 2^(J - j - 1), L)
      wcb[1:2^(J - j - 1), (2^(J - j - 1) + 1):2^(J - j)] <- LH
    }
  }
  if (thresh == "hard") {
    for (j in (0:(J - 1 - j0))) {
      HH <- wc[(2^(J - j - 1) + 1):2^(J - j), (2^(J - j - 1) + 1):2^(J - j)]
      HH <- block_partition2d(HH, L)
      HHmask <- apply(HH^2, 2, sum) >= (lamb * hatsigma^2 * L)
      HH <- HH * repmat(HHmask, L * L, 1)
      HH <- invblock_partition2d(HH, 2^(J - j - 1), L)
      wcb[(2^(J - j - 1) + 1):2^(J - j), (2^(J - j - 1) + 1):2^(J - j)] <- HH

      HL <- wc[(2^(J - j - 1) + 1):2^(J - j), (1:2^(J - j - 1))]
      HL <- block_partition2d(HL, L)
      HLmask <- apply(HL^2, 2, sum) >= (lamb * hatsigma^2 * L)
      HL <- HL * repmat(HLmask, L * L, 1)
      HL <- invblock_partition2d(HL, 2^(J - j - 1), L)
      wcb[(2^(J - j - 1) + 1):2^(J - j), 1:2^(J - j - 1)] <- HL

      LH <- wc[1:2^(J - j - 1), (2^(J - j - 1) + 1):2^(J - j)]
      LH <- block_partition2d(LH, L)
      LHmask <- apply(LH^2, 2, sum) >= (lamb * hatsigma^2 * L)
      LH <- LH * repmat(LHmask, L * L, 1)
      LH <- invblock_partition2d(LH, 2^(J - j - 1), L)
      wcb[1:2^(J - j - 1), (2^(J - j - 1) + 1):2^(J - j)] <- LH
    }
  }
  return(wcb)
}
