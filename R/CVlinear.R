#' 2-Fold Cross Validation for linear estimator
#'
#' Selection of the number of wavelet coefficients to be maintained by the
#' cross validation method proposed by Nason in the case of threshold
#' selection. This method is adapted here to select among linear estimators.
#'
#' @export CVlinear
#' @param Y Noisy observations.
#' @param L Level of coarsest scale.
#' @param qmf Orthonormal quadrature mirror filter.
#' @param D Dimension vector of the models considered.
#' @param wc 1-d wavelet coefficients.
#' @return \code{CritCV} Cross validation criteria.
#' @return \code{hat_f_m_2FCV}
#' @references
#' Nason, G. P. (1996). Wavelet shrinkage using cross-validation. \emph{Journal of the Royal Statistical Society: Series B}, 58(2), 463-479.
#'
#' Navarro, F. and Saumard, A. (2017). Slope heuristics and V-Fold model selection in heteroscedastic regression using strongly localized bases. \emph{ESAIM: Probability and Statistics}, 21, 412-451.

CVlinear <- function(Y, L, qmf, D, wc) {
  n <- length(Y)
  n1 <- n/2
  lD <- length(D)
  wc_odd_lin <- rep(0, n1)
  wc_even_lin <- rep(0, n1)
  Y_odd <- Y[seq(1, n, 2)]
  Y_even <- Y[seq(2, n, 2)]

  wc_odd <- FWT_PO(Y_odd, L, qmf)
  wc_even <- FWT_PO(Y_even, L, qmf)
  hat_f_odd <- matrix(0, nrow = lD, ncol = n1)
  hat_f_even <- matrix(0, nrow = lD, ncol = n1)
  hat_f_m <- matrix(rep(0, lD * n), nrow = lD, ncol = n)
  wc_hat_f_m <- rep(0, n)
  for (i in 1:length(D)) {
    wc_hat_f_m[1:(2^i)] <- wc[1:(2^i)]
    hat_f_m[i, ] <- IWT_PO(wc_hat_f_m, L, qmf)

    wc_odd_lin[1:(2^i)] <- wc_odd[1:(2^i)]
    wc_even_lin[1:(2^i)] <- wc_even[1:(2^i)]
    hat_f_odd[i, ] <- IWT_PO(wc_odd_lin, L, qmf)
    hat_f_even[i, ] <- IWT_PO(wc_even_lin, L, qmf)
  }
  bar_f_odd <- 0.5 * (hat_f_odd[, 1:(n1 - 1)] + hat_f_odd[, 2:n1])
  bar_f_odd <- cbind(bar_f_odd, hat_f_odd[, 1])
  bar_f_even <- 0.5 * (hat_f_even[, 1:(n1 - 1)] + hat_f_even[, 2:n1])
  bar_f_even <- cbind(bar_f_even, hat_f_even[, 1])

  mat_Y_odd <- repmat(Y_odd, length(D), 1)
  mat_Y_even <- repmat(Y_even, length(D), 1)

  tmp <- (mat_Y_odd - bar_f_even)^2 + (mat_Y_even - bar_f_odd)^2
  Crit_CV <- rowSums(tmp)
  hat_m_2FCV <- which.min(Crit_CV)
  hat_f_m_2FCV <- hat_f_m[hat_m_2FCV, ]
  return(list("CritCV" = Crit_CV, "hat_f_m_2FCV" = hat_f_m_2FCV))
}

