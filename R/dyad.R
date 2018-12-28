#' Index entire j-th dyad of 1-d wavelet xform
#'
#' @export dyad
#' @param j integer.
#' @return \code{ix} list of all indices of wavelet coeffts at j-th level.
#' @examples
#' dyad(0)

dyad <- function(j) {
  i <- (2^(j) + 1):(2^(j + 1))
  return(i)
}

# Copyright (c) 1993. David L. Donoho

# Part of Wavelab Version 850 Built Tue Jan 3 13:20:40 EST 2006 This is
# Copyrighted Material For Copying permissions see COPYING.m Comments? e-mail
# wavelab@stat.stanford.edu
