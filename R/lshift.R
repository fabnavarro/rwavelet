#' Circular left shift of 1-d signal
#'
#' @export lshift
#' @param a 1-d signal.
#' @return \code{l} 1-d signal l(i) = x(i+1) except l(n) = x(1).
#' @examples
#' \dontrun{
#' lshift(x)
#' }

lshift <- function(a) {
  return(c(a[2:length(a)], a[1]))
}
