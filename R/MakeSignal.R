#' Make artificial signal.
#'
#' @export MakeSignal
#' @param name string, 'HeaviSine', 'Bumps', 'Blocks',
#'             'Doppler', 'Ramp','Cusp', 'Sing'.
#' @param n desired signal length
#' @return \code{sig} 1-d signal.
#' @examples
#' \dontrun{
#' sig <- MakeSignal(Name,n)
#' }
#' @seealso \code{\link{FWT_PO}}, \code{\link{IWT_PO}}, \code{\link{FWT2_PO}}, \code{\link{IWT2_PO}}.


MakeSignal <- function(name, n) {
  t <- (1:n)/n
  if (name == "HeaviSine") {
    sig <- 4 * sin(4 * pi * t)
    sig <- sig - sign(t - 0.3) - sign(0.72 - t)
    return(sig)
  }
  if (name == "Bumps") {
    pos <- c(0.1, 0.13, 0.15, 0.23, 0.25, 0.4, 0.44, 0.65, 0.76, 0.78, 0.81)
    hgt <- c(4, 5, 3, 4, 5, 4.2, 2.1, 4.3, 3.1, 5.1, 4.2)
    wth <- c(0.005, 0.005, 0.006, 0.01, 0.01, 0.03, 0.01, 0.01, 0.005, 0.008,
      0.005)
    sig <- 2 * rep(0, length(t))
    for (j in 1:length(pos)) {
      sig <- sig + hgt[j]/((1 + (abs(t - pos[j])/wth[j]))^4)
    }
    return(sig)
  }
  if (name == "Blocks") {
    pos <- c(0.1, 0.13, 0.15, 0.23, 0.25, 0.4, 0.44, 0.65, 0.76, 0.78, 0.81)
    hgt <- c(4, -5, 3, -4, 5, -4.2, 2.1, 4.3, -3.1, 2.1, -4.2)
    sig <- 2 * rep(1, length(t))
    for (j in 1:length(pos)) {
      sig <- sig + (1 + sign(t - pos[j])) * (hgt[j]/2)
    }
    return(sig)
  }
  if (name == "Doppler") {
    sig <- sqrt(t * (1 - t)) * sin((2 * pi * 1.05)/(t + 0.05)) + 0.5
    return(sig)
  }
  if (name == "Ramp") {
    sig <- t - (t >= 0.37)
    return(sig)
  }
  if (name == "Cusp") {
    sig <- sqrt(abs(t - 0.37))
    return(sig)
  }
  if (name == "Sing") {
    k <- floor(n * 0.37)
    sig <- 1/abs(t - (k + 0.5)/n)
    return(sig)
  } else {
    print("Allowable Names are:")
    print("HeaviSine")
    print("Bumps")
    print("Blocks")
    print("Doppler")
    print("Ramp")
    print("Cusp")
    print("Sing")
  }
}
