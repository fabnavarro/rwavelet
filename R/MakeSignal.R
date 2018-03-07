#' Make artificial signal.
#'
#' @export MakeSignal
#' @param name string, 'HeaviSine', 'Bumps', 'Blocks',
#'             'Doppler', 'Ramp','Cusp', 'Sing', 'HiSine',
#'             'LoSine', 'LinChirp', 'TwoChirp', 'QuadChirp',
#'             'MishMash', 'WernerSorrows' (Heisenberg),
#'             'Leopold' (Kronecker), 'Riemann', 'HypChirps'
#'
#' @param n desired signal length
#' @return \code{sig} 1-d signal.
#' @examples
#' name <- 'Cusp'
#' n <- 2^5
#' sig <- MakeSignal(name,n)
#' @seealso \code{\link{FWT_PO}}, \code{\link{IWT_PO}}, \code{\link{FWT2_PO}},
#' \code{\link{IWT2_PO}}.


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
  }
  if (name == "HiSine") {
    sig <- sin(pi * (n * 0.6902) * t)
    return(sig)
  }
  if (name == "LoSine") {
    sig <- sin(pi * (n * 0.3333) * t)
    return(sig)
  }
  if (name == "LinChirp") {
    sig <- sin(pi * t * ((n * 0.5) * t))
    return(sig)
  }
  if (name == "TwoChirp") {
    sig <- sin(pi * t * (n * t)) + sin((pi/3) * t * (n * t))
    return(sig)
  }
  if (name == "QuadChirp") {
    sig <- sin((pi/3) * t * (n * t^2))
    return(sig)
  }
  if (name == "MishMash") {
    # QuadChirp + LinChirp + HiSine
    sig <- sin((pi/3) * t * (n * t^2))
    sig <- sig + sin(pi * (n * 0.6902) * t)
    sig <- sig + sin(pi * t * (n * 0.125 * t))
    return(sig)
  }
  if (name == "WernerSorrows") {
    sig <- sin(pi * t * (n/2 * t^2))
    sig <- sig + sin(pi * (n * 0.6902) * t)
    sig <- sig + sin(pi * t * (n * t))
    pos <- c(0.1, 0.13, 0.15, 0.23, 0.25, 0.4, 0.44, 0.65, 0.76, 0.78, 0.81)
    hgt <- c(4, 5, 3, 4, 5, 4.2, 2.1, 4.3, 3.1, 5.1, 4.2)
    wth <- c(0.005, 0.005, 0.006, 0.01, 0.01, 0.03, 0.01, 0.01, 0.005, 0.008, 
      0.005)
    for (j in 1:length(pos)) {
      sig <- sig + hgt[j]/(1 + abs((t - pos[j])/wth[j]))^4
    }
    return(sig)
  }
  if (name == "Leopold") {
    sig <- (t == floor(0.37 * n)/n)  # Kronecker
    return(sig)
  }
  if (name == "Riemann") {
    sqn <- round(sqrt(n))
    sig <- t * 0  # Riemann's Non-differentiable Function
    sig[(1:sqn)^2] <- 1/(1:sqn)
    return(sig)
  }
  if (name == "HypChirps") {
    alpha <- 15 * n * pi/1024
    beta <- 5 * n * pi/1024
    t <- (1.001:(n + 0.001))/n
    f1 <- rep(0, n)
    f2 <- rep(0, n)
    f1 <- sin(alpha/(0.8 - t)) * (0.1 < t) * (t < 0.68)
    f2 <- sin(beta/(0.8 - t)) * (0.1 < t) * (t < 0.75)
    M <- round(0.65 * n)
    P <- floor(M/4)
    enveloppe <- rep(1, M)  # the rising cutoff function
    enveloppe[1:P] <- (1 + sin(-pi/2 + ((1:P) - rep(1, P))/(P - 1) * pi))/2
    enveloppe[(M - P + 1):M] <- rev(enveloppe[1:P])
    env <- rep(1, n)
    env[ceiling(n/10):(M + ceiling(n/10) - 1)] <- enveloppe[1:M]
    sig <- (f1 + f2) * env
  } else {
    cat("Allowable Names are:")
    cat("HeaviSine")
    cat("Bumps")
    cat("Blocks")
    cat("Doppler")
    cat("Ramp")
    cat("Cusp")
    cat("Sing")
    cat("HiSine")
    cat("LoSine")
    cat("LinChirp")
    cat("TwoChirp")
    cat("QuadChirp")
    cat("MishMash")
    cat("WernerSorrows")
    cat("Leopold")
    cat("Riemann")
    cat("HypChirps")
  }
}
