#' Make artificial 1-d signal.
#'
#' @export MakeSignalNewb
#' @import utils
#' @param name string, 'Cusp','Step','Wave','Blip','Blocks',
#'           'Bumps','HeaviSine','Doppler','Angles',
#'          'Parabolas','Time Shifted Sine','Spikes','Corner'
#' @param n desired signal length.
#' @return \code{sig} 1-d signal.
#' @examples
#' name <- 'Cusp'
#' n <- 2^5
#' sig <- MakeSignalNewb(name,n)
#' @seealso \code{\link{FWT_PO}}, \code{\link{IWT_PO}}, \code{\link{FWT2_PO}},
#' \code{\link{IWT2_PO}}.

MakeSignalNewb <- function(name, n) {
  t <- (1:n)/n
  if (name == "Step") {
    sig <- 0.2 + 0.6 * (t > 1/3 & t <= 0.75)
    return(sig)
  }
  if (name == "Wave") {
    sig <- 0.5 + (0.2 * cos(4 * pi * t)) + (0.1 * cos(24 * pi * t))
    return(sig)
  }
  if (name == "Blip") {
    sig <- (0.32 + (0.6 * t) + 0.3 * exp(-100 * ((t - 0.3)^2))) * (t <= 0.8) +
      (-0.28 + (0.6 * t) + 0.3 * exp(-100 * ((t - 1.3)^2))) * (t > 0.8)
    return(sig)
  }
  if (name == "Blocks") {
    pos <- c(0.1, 0.13, 0.15, 0.23, 0.25, 0.4, 0.44, 0.65, 0.76, 0.78, 0.81)
    hgt <- c(4, -5, 3, -4, 5, -4.2, 2.1, 4.3, -3.1, 2.1, -4.2)
    sig <- 2 * rep(1, length(t))
    for (j in 1:length(pos)) {
      sig <- sig + (1 + sign(t - pos[j])) * (hgt[j]/2)
    }
    sig <- (0.6/9.2) * sig + 0.2
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
    sig <- ((0.6/5.3437952) * sig) + 0.2
    return(sig)
  }
  if (name == "HeaviSine") {
    sig <- 4 * sin(4 * pi * t) - sign(t - 0.3) - sign(0.72 - t) + 5
    sig <- (0.6/9) * sig + 0.2
    return(sig)
  }
  if (name == "Doppler") {
    sig <- sqrt(t * (1 - t)) * sin((2 * pi * 1.05)/(t + 0.05)) + 0.5
    sig <- 0.6 * sig + 0.2
    return(sig)
  }
  if (name == "Angles") {
    sig <- ((2 * t + 0.5) * (t <= 0.15)) + ((-12 * (t - 0.15) + 0.8) * (t >
      0.15 & t <= 0.2)) + 0.2 * (t > 0.2 & t <= 0.5) + ((6 * (t - 0.5) +
      0.2) * (t > 0.5 & t <= 0.6)) + ((-10 * (t - 0.6) + 0.8) * (t > 0.6 &
      t <= 0.65)) + ((-0.5 * (t - 0.65) + 0.3) * (t > 0.65 & t <= 0.85)) +
      ((2 * (t - 0.85) + 0.2) * (t > 0.85))
    return(sig)
  }
  if (name == "Parabolas") {
    pos <- c(0.1, 0.2, 0.3, 0.35, 0.37, 0.41, 0.43, 0.5, 0.7, 0.9)
    hgt <- c(-30, 60, -30, 500, -1000, 1000, -500, 7.5, -15, 7.5)
    sig <- 2 * rep(0, length(t))
    for (j in 1:length(pos)) {
      sig <- sig + hgt[j] * ((t - pos[j])^2) * (t > pos[j])
    }
    sig <- sig + 0.8
    return(sig)
  }
  if (name == "Time Shifted Sine") {
    u <- t
    for (j in (1:4)) {
      u <- 0.5 * (1 - cos(pi * u))
    }
    sig <- 0.3 * sin(3 * pi * (u + t)) + 0.5
    return(sig)
  }
  if (name == "Spikes") {
    sig <- 15.6676 * (exp(-500 * (t - 0.23)^2) + 2 * exp(-2000 * (t - 0.33)^2) +
      4 * exp(-8000 * (t - 0.47)^2) + 3 * exp(-16000 * (t - 0.69)^2) +
      exp(-32000 * (t - 0.83)^2))
    sig <- (0.6/diff(range(sig))) * sig + 0.2
    return(sig)
  }
  if (name == "Corner") {
    sig <- t
    sig[t <= 0.5] <- 62.387 * 10 * t[t <= 0.5]^3 * (1 - 4 * t[t <= 0.5]^2)
    sig[0.5 < t & t <= 0.8] <- 62.387 * 3 * (0.125 - t[0.5 < t & t <= 0.8]^3) *
      t[0.5 < t & t <= 0.8]^4
    sig[t > 0.8] <- 62.387 * 59.443 * (t[t > 0.8] - 1)^3
    sig <- (0.6/diff(range(sig))) * sig + 0.6
    return(sig)
  } else {
    print("Allowable Names are:")
    print("Step")
    print("Wave")
    print("Blip")
    print("Blocks")
    print("Bumps")
    print("HeaviSine")
    print("Doppler")
    print("Angles")
    print("Parabolas")
    print("Time Shifted Sine")
    print("Spikes")
    print("Corner")
  }
}

# Acknowledgement This code is based on a code provided by Buckheit, Chen,
# Donoho, Johnstone & Scargle.  Copyright (c) 2001 Anestis Antoniadis,
# Jeremie Bigot Laboratoire IMAG-LMC University Joseph Fourier BP 53, 38041
# Grenoble Cedex 9 France.  mailto: Anestis.Antoniadis@imag.fr mailto:
# Jeremie.Bigot@imag.fr and Theofanis Sapatinas Department of Mathematics and
# Statistics University of Cyprus P.O. Box 20537 CY 1678 Nicosia Cyprus.
# mailto: T.Sapatinas@ucy.ac.cy
