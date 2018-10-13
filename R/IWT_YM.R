#' Inverse Wavelet Transform (periodized Meyer Wavelet)
#'
#' The Meyer wavelet is defined in the frequency domain.
#' The algorithm is very different from usual quadrature
#' mirror filter algorithms.  See the Ph. D. Thesis of
#' Eric Kolaczyk.
#'
#' @export FWT_YM
#' @param wc 1-d wavelet transform, length(wc) = 2^J.
#' @param L Coarsest Level of V_0;  L << J.
#' @param deg  degree of polynomial window 2 <= deg <=4.
#' @return \code{w} 1-d reconstructed signal; length(x) = 2^J.
#' @examples
#' \dontrun{
#' x <- IWT_YM(wc, L, deg)
#' }
#' @seealso \code{\link{FWT_YM}}, \code{\link{CoarseMeyerProj}},
#' \code{\link{DetailMeyerProj}}, \code{\link{FineMeyerProj}}.
