% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IWT_TI.R
\name{IWT_TI}
\alias{IWT_TI}
\title{Invert Translation Invariant Wavelet Transform}
\usage{
IWT_TI(pkt, qmf)
}
\arguments{
\item{pkt}{translation-invariant wavelet transform table (TIWT).}

\item{qmf}{orthonormal quadrature mirror filter.}
}
\value{
\code{x} 1-d signal reconstructed from translation-invariant transform TIWT.
}
\description{
Invert Translation Invariant Wavelet Transform
}
\examples{
x <- MakeSignal('Ramp', 8)
L <- 0
qmf <- MakeONFilter('Haar')
TIWT <- FWT_TI(x, L, qmf)
xr <- IWT_TI(TIWT,qmf)
}
\seealso{
\code{\link{FWT_TI}}, \code{\link{MakeONFilter}}.
}
