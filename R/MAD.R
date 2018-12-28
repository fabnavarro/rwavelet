#' Median Absolute Deviation
#'
#' Compute the median absolute deviation.
#'
#' @export MAD
#' @param x 1-d signal.
#' @examples
#' x <- c(1, 1, 2, 2, 4, 6, 9)
#' MAD(x)

MAD <- function(x) {
  return(median(abs(x - median(x)))/0.6745)
}

# Part of Wavelab Version 850 Built Tue Jan 3 13:20:41 EST 2006 This is
# Copyrighted Material For Copying permissions see COPYING.m Comments? e-mail
# wavelab@stat.stanford.edu
