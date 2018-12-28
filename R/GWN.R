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

# Written by Maureen Clerc and Jerome Kalifa, 1997
# clerc@cmapx.polytechnique.fr, kalifa@cmapx.polytechnique.fr

# Part of Wavelab Version 850 Built Tue Jan 3 13:20:39 EST 2006 This is
# Copyrighted Material For Copying permissions see COPYING.m Comments? e-mail
# wavelab@stat.stanford.edu
