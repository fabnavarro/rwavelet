#' Generation of Gaussian White Noise.
#'
#' @export GWN
#' @param n sample size.
#' @param sigma standard deviation.
#' @return \code{epsilon} resulting noise.
#' @examples
#' GWN(10,0.1)

GWN <- function(n, sigma) {
  epsilon <- rnorm(n = n, mean = 0, sd = sigma)
  return(epsilon)
}
