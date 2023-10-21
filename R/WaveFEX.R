#' Wavelet-based Feature EXtraction
#'
#' Todo: add description
#'
#' @export WaveFEX
#' @param type Orthogonal transform: PO or translation invariant transform:TI.
#' @param obs A matrix of n x p functional data (n should be a power of 2).
#' @param qmf Orthonormal quadrature mirror filter.
#' @param scale Finest scale of the decomposition.
#' @param iwt Logical to specify forward (FALSE) or inverse (TRUE) wavelet transform.
#' @param alpha Logical TRUE to include scaling coefficients at the first scale.
#' @return \code{contj} Matrix  scale x p of Squared norm of the wavelet coefficient at each scale.
#' @return \code{wc} wavelet coefficients matrix.
#' @references
#' Antoniadis, A., Brossat, X., Cugliari, J., & Poggi, J. M. (2013). Clustering functional data using wavelets. \emph{Int. J. Wavelets Multiresolution Inf. Process.}, 11(01), 1350003.
#'
#' Cheam, A., Fredette, M., Marbac, M., & Navarro, F. (2023). Translation-invariant functional clustering on COVID-19 deaths adjusted on population risk factors. \emph{J. R. Stat. Soc., C: Appl. Stat.}, 72(2), 387-413.

WaveFEX <- function(type="PO", obs, qmf, scale=log2(nrow(obs)), iwt=F, alpha=F)
  switch(type,
         TI = waveletsTI(obs, qmf, scale, iwt, alpha),
         PO = waveletsPO(obs, qmf, scale, iwt, alpha))

waveletsPO <- function(obs, qmf, scale, iwt, alpha){
  is_pow2(obs)
  J <- floor(log2(nrow(obs)))
  n <- nrow(obs)
  j0 <- 0
  norm2vec <- function(x){sum(x^2)}
  wc <- apply(obs,2,FWT_PO,j0,qmf)
  # Linear estimator collection
  wci <- array(0,dim=dim(wc))
  wci[1:(2^scale),] <- wc[1:(2^scale),]
  if(iwt){wcr <- apply(wci, 2, IWT_PO, j0, qmf)}
  # Energy criteria
  z_i <- wci[-1,]
  contj <- matrix(0, ncol = ncol(z_i), nrow = scale)
  if(alpha){contj[1,] <- z_i[1,]^2 + wc[1,]^2}
  if(!alpha){contj[1,] <- z_i[1,]^2}
  for (j in 2:scale){
    ind <- (2^(j-1)):(2^j-1)
    contj[j,] <- apply(z_i[ind,],2, norm2vec)
  }
  if(iwt){
    list(contj = contj,
         wci = wci,
         wcr = wcr)}
  if(!iwt){
    list(contj = contj,
         wci = wci)}
}

waveletsTI <- function(obs, qmf, scale, iwt, alpha){
  is_pow2(obs)
  J <- floor(log2(nrow(obs)))
  df <- obs[1:2^J,]
  n <- nrow(df)
  j0 <- 0
  norm2vec <- function(x){sum(x^2)}
  wc <- apply(df, 2, FWT_TI, j0, qmf)
  wci <- array(0, dim=dim(wc))
  for (jj in 1:(scale+1)){
    ind <- ((jj-1)*n+1):(n*jj)
    wci[ind,] <- wc[ind,]
  }
  wctmp <- wci
  if(iwt){
    # reshape wci to match TItable for inversion
    wci <- lapply(1:ncol(wci),
                  function(i){array(wci[,i], dim=c(n,J+1))})
    # go back to original domain by inversion
    wcr <- array(unlist(lapply(wci,IWT_TI,qmf)),dim=dim(df))
  }
  if (alpha){
    z_i <- wctmp
    contj <- matrix(0,ncol = ncol(z_i),
                            nrow = scale+1)
    for (j in 1:(scale+1)){
      ind <- ((j-1)*n+1):(n*j)
      contj[j,] <- apply(z_i[ind,], 2, norm2vec)
    }
  }
  if (!alpha){
    z_i <- wctmp[-c(1:n),]
    contj <- matrix(0,ncol = ncol(z_i),
                            nrow = scale)
    for (j in 1:scale){
      ind <- ((j-1)*n+1):(n*j)
      contj[j,] <- apply(z_i[ind,], 2, norm2vec)
    }
  }
  if(iwt){
    list(contj = contj,
         wci = wci,
         wcr = wcr)}
  if(!iwt){
    list(contj = contj,
         wci = wci)}
}

is_pow2 <- function(x) {
  if (!is.matrix(x)) {
    stop("Input is not a matrix.")
  }
  log_val <- log2(dim(x))
  if (log_val[1] != floor(log_val[1])) {
    stop("The first dimension should be a power of two.")
  }
  # Check if none of the dimensions are a power of 2
  if (all(log_val != floor(log_val))) {
    stop("None of the dimensions are a power of two.")
  }
  return(log_val == floor(log_val))
}
