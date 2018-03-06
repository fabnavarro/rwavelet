#' Apply Hard Threshold.
#'
#' @export HardThresh
#' @param y Noisy Data.
#' @param t Threshold.
#' @return \code{x} filtered result (y 1_{|y|>t}).
#' @examples
#' \dontrun{
#' HardThresh(y,t)
#' }
#' @seealso \code{\link{iconv}}, \code{\link{UpDyadHi}},

HardThresh <- function(y, t) {
  x <- y * (abs(y) > t)
  return(x)
}
