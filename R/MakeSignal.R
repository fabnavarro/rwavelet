#' Make artificial signal
#'
#' @export MakeSignal
#' @import utils
#' @param name string, 'HeaviSine', 'Bumps', 'Blocks',
#'             'Doppler', 'Ramp','Cusp', 'Sing', 'HiSine',
#'             'LoSine', 'LinChirp', 'TwoChirp', 'QuadChirp',
#'             'MishMash', 'WernerSorrows' (Heisenberg),
#'             'Leopold' (Kronecker), 'Riemann', 'HypChirps',
#'             'LinChirps', 'Chirps', 'Gabor', 'sineoneoverx',
#'             'Cusp2', 'SmoothCusp', 'Piece-Regular' (Piece-Wise Smooth),
#'             'Piece-Polynomial' (Piece-Wise 3rd degree polynomial).
#' @param n desired signal length.
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
    # Hyperbolic Chirps of Mallat's book
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
  }
  if (name == "LinChirps") {
    # Linear Chirps of Mallat's book
    b <- 100 * n * pi/1024
    a <- 250 * n * pi/1024
    t <- (1:n)/n
    A1 <- sqrt((t - 1/n) * (1 - t))
    sig <- A1 * (cos((a * (t)^2)) + cos((b * t + a * (t)^2)))
  }
  if (name == "Chirps") {
    # Linear Chirps of Mallat's book
    t <- (1:n)/n * 10 * pi
    f1 <- cos(t^2 * n/1024)
    a <- 30 * n/1024
    t <- (1:n)/n * pi
    f2 <- cos(a * (t^3))
    f2 <- rev(f2)
    ix <- (-n:n)/n * 20
    g <- exp(-ix^2 * 4 * n/1024)
    i1 <- ((n/2 + 1):(n/2 + n))
    i2 <- ((n/8 + 1):(n/8 + n))
    j <- (1:n)/n
    f3 <- g[i1] * cos(50 * pi * j * n/1024)
    f4 <- g[i2] * cos(350 * pi * j * n/1024)
    sig <- f1 + f2 + f3 + f4
    enveloppe <- rep(1, n)  # the rising cutoff function
    enveloppe[1:(n/8)] <- (1 + sin(-pi/2 + ((1:(n/8)) - rep(1, n/8))/(n/8 -
      1) * pi))/2
    enveloppe[(7 * n/8 + 1):n] <- rev(enveloppe[1:(n/8)])
    sig <- sig * enveloppe
  }
  if (name == "sineoneoverx") {
    # sin(1/x) in Mallat's book
    N <- 1024
    i1 <- ((-N + 1):N)
    i1[N] <- 1/100
    i1 <- i1/(N - 1)
    sig <- sin(1.5/(i1))
    sig <- sig[513:1536]
  }
  if (name == "Cusp2") {
    N <- 64
    i1 <- (1:N)/N
    x <- (1 - sqrt(i1)) + i1/2 - 0.5
    M <- 8 * N
    sig <- rep(0, M)
    sig[(M - 1.5 * N + 1):(M - 0.5 * N)] <- x
    sig[(M - 2.5 * N + 2):(M - 1.5 * N + 1)] <- rev(x)
    sig[(3 * N + 1):(3 * N + N)] <- 0.5 * rep(1, N)
    sig
  }
  # ToDo: trad fftshift from matlab
  if (name == "SmoothCusp") {
    N <- 64
    i1 <- (1:N)/N
    x <- (1 - sqrt(i1)) + i1/2 - 0.5
    M <- 8 * N
    sig <- rep(0, M)
    sig[(M - 1.5 * N + 1):(M - 0.5 * N)] <- x
    sig[(M - 2.5 * N + 2):(M - 1.5 * N + 1)] <- rev(x)
    sig[(3 * N + 1):(3 * N + N)] <- 0.5 * rep(1, N)
    N <- 64
    M <- 8 * N
    t <- (1:M)/M
    sigma <- 0.01
    g <- exp(-0.5 * (abs(t - 0.5)/sigma)^2)/sigma/sqrt(2 * pi)
    g <- c(tail(g, ceiling(length(g)/2)), head(g, ceiling(length(g)/2)))
    sig2 <- iconvv(g, sig)/M
  }
  if (name == "Piece-Regular") {
    sig1 <- -15 * MakeSignal("Bumps", n)
    t <- (1:trunc(n/12))/trunc(n/12)
    sig2 <- -exp(4 * t)
    t <- (1:trunc(n/7))/trunc(n/7)
    sig5 <- exp(4 * t) - exp(4)
    t <- (1:trunc(n/3))/trunc(n/3)
    sigma <- 6/40
    sig6 <- -70 * exp(-((t - 1/2) * (t - 1/2))/(2 * sigma^2))
    sig <- rep(0, n)
    sig[1:trunc(n/7)] <- sig6[1:trunc(n/7)]
    sig[(trunc(n/7) + 1):trunc(n/5)] <- 0.5 * sig6[(trunc(n/7) + 1):trunc(n/5)]
    sig[(trunc(n/5) + 1):trunc(n/3)] <- sig6[(trunc(n/5) + 1):trunc(n/3)]
    sig[(trunc(n/3) + 1):trunc(n/2)] <- sig1[(trunc(n/3) + 1):trunc(n/2)]
    sig[(trunc(n/2) + 1):(trunc(n/2) + trunc(n/12))] <- sig2
    sig[(trunc(n/2) + 2 * trunc(n/12)):(trunc(n/2) + trunc(n/12) + 1)] <- sig2
    ind <- trunc(n/2) + 2 * trunc(n/12) + 3 * trunc(n/20) - trunc(n/2) -
      2 * trunc(n/12) - trunc(n/20)
    if (ind != 0) {
      sig[(trunc(n/2) + 2 * trunc(n/12) + trunc(n/20) + 1):(trunc(n/2) +
        2 * trunc(n/12) + 3 * trunc(n/20))] <- -rep(1, trunc(n/2) + 2 *
        trunc(n/12) + 3 * trunc(n/20) - trunc(n/2) - 2 * trunc(n/12) -
        trunc(n/20)) * 25
    }
    k <- trunc(n/2) + 2 * trunc(n/12) + 3 * trunc(n/20)
    sig[(k + 1):(k + trunc(n/7))] <- sig5
    diff <- n - 5 * trunc(n/5)
    if (diff != 0) {
      sig[(5 * trunc(n/5) + 1):n] <- sig[rev(seq_len(diff))]
    }
    # zero-mean
    bias <- sum(sig)/n
    sig <- bias - sig
  }
  if (name == "Piece-Polynomial") {
    t <- (1:trunc(n/5))/trunc(n/5)
    sig1 <- 20 * (t^3 + t^2 + 4)
    sig3 <- 40 * (2 * t^3 + t) + 100
    sig2 <- 10 * t^3 + 45
    sig4 <- 16 * t^2 + 8 * t + 16
    sig5 <- 20 * (t + 4)
    sig6[1:trunc(n/10)] <- rep(1, trunc(n/10))
    sig6 <- sig6 * 20
    sig <- rep(0, n)
    sig[1:trunc(n/5)] <- sig1
    sig[(2 * trunc(n/5)):(trunc(n/5) + 1)] <- sig2
    sig[(2 * trunc(n/5) + 1):(3 * trunc(n/5))] <- sig3
    sig[(3 * trunc(n/5) + 1):(4 * trunc(n/5))] <- sig4
    sig[(4 * trunc(n/5) + 1):(5 * trunc(n/5))] <- sig5[trunc(n/5):1]
    diff <- n - 5 * trunc(n/5)
    if (diff != 0) {
      sig[(5 * trunc(n/5) + 1):n] <- sig[diff:1]
    }
    sig[(trunc(n/20) + 1):(trunc(n/20) + trunc(n/10))] <- rep(1, trunc(n/10)) *
      10
    if (trunc(n/20) != 0) {
      sig[(n - trunc(n/10) + 1):(n + trunc(n/20) - trunc(n/10))] <- rep(1,
        trunc(n/20)) * 150
    }
    # zero-mean
    bias <- sum(sig)/n
    sig <- sig - bias
  }
  if (name == "Gabor") {
    # two modulated Gabor functions in Mallat's book
    N <- 512
    sig <- rep(0, N)
    t <- (-N:N) * 5/N
    j <- (1:N)/N
    g <- exp(-t^2 * 20)
    i1 <- ((2 * N/4 + 1):(2 * N/4 + N))
    i2 <- ((N/4 + 1):(N/4 + N))
    sig1 <- 3*g[i1]*exp(1i*N/16*pi*j)
    sig2 <- 3*g[i2]*exp(1i*N/4*pi*j)
    sig <- sig1 + sig2
    sig <- Re(sig)
  } else {
    warning("Allowable Names are listed above")
    print("HeaviSine")
    print("Bumps")
    print("Blocks")
    print("Doppler")
    print("Ramp")
    print("Cusp")
    print("Sing")
    print("HiSine")
    print("LoSine")
    print("LinChirp")
    print("TwoChirp")
    print("QuadChirp")
    print("MishMash")
    print("WernerSorrows")
    print("Leopold")
    print("Riemann")
    print("HypChirps")
    print("LinChirps")
    print("Chirps")
    print("Gabor")
    print("sineoneoverx")
    print("Cusp2")
    print("SmoothCusp")
    print("Piece-Regular")
    print("Piece-Polynomial")
  }
}

# Originally made by David L. Donoho.  Function has been enhanced.

# Part of Wavelab Version 850 Built Tue Jan 3 13:20:39 EST 2006 This is
# Copyrighted Material For Copying permissions see COPYING.m Comments? e-mail
# wavelab@stat.stanford.edu
