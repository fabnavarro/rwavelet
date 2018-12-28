#' Packet table indexing.
#'
#' @export packet
#' @param d depth of splitting in packet decomposition.
#' @param b block index among 2^d possibilities at depth d.
#' @param n length of signal
#' @return \code{p} linear indices of all coeff's in that block.
#' @examples
#' packet(1, 1, 8)

packet <- function(d, b, n) {
  npack <- 2^d
  p <- ((b * (n/npack) + 1):((b + 1) * n/npack))
  return(p)
}

# Copyright (c) 1993. David L. Donoho

# Part of Wavelab Version 850 Built Tue Jan 3 13:20:40 EST 2006 This is
# Copyrighted Material For Copying permissions see COPYING.m Comments? e-mail
# wavelab@stat.stanford.edu
