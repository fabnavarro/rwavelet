rwavelet
========

[![Build Status](https://travis-ci.org/fabnavarro/rwavelet.svg)](https://travis-ci.org/fabnavarro/rwavelet)

Wavelet analysis in R

Download and Install
--------------------

Install the devtools package if you haven't already.

``` r
install.packages("devtools")
```

To install the package, type the following at the R command line:

``` r
devtools::install_github("fabnavarro/rwavelet")
library(rwavelet)
```

Getting started
---------------

Denoising of an experimental nuclear magnetic resonance (NMR) spectrum

``` r
data("RaphNMR")
j0 <- 0
Y <- RaphNMR
n <- length(Y)
t <- seq(0, 1, length = n)
J <- log2(n)

Ywd <- FWT_PO(Y,j0,qmf)
Ywnoise <- Ywd

# estimate sigma using the maximum absolute deviation
# (using only the finest scale coefficients)
hatsigma <- MAD(Ywd[(2^(J-1)+1):2^J])
# universal thresholding
lambda <- sqrt(2*log(n))*hatsigma
# apply hard thresholding 
Ywd[(2^(j0)+1):n] <- HardThresh(Ywd[(2^(j0)+1):n],lambda)

par(mfrow=c(2,2))
plot(t,Y,xlab="",ylab="")
plot(t,Y,xlab="",ylab="")
matlines(t, fhat, lwd=2, col="blue", lty=1)
plot(Ywnoise, ylim=c(-20, 20), xlab="", ylab="")
matlines(rep(lambda,n), lwd=2,col="red",lty=1)
matlines(-rep(lambda,n), lwd=2,col="red",lty=1)
plot(Ywd, ylim=c(-20,20), xlab="", ylab="")
```

See the [package vignette](http://fnavarro.perso.math.cnrs.fr/rpackage/rwaveletvignette.html) for more details. You could also build and see the vignette associated with the package using the following lines of code

``` r
devtools::install_github("fabnavarro/rwavelet", build_vignettes = TRUE)
library(rwavelet)
```

Then, to view the vignette

``` r
vignette("rwaveletvignette")
```

How to cite
-----------

``` r
citation("rwavelet")
#> 
#> To cite rwavelet in publications use:
#> 
#>   F. Navarro and C. Chesneau (2018). R package rwavelet: Wavelet
#>   analysis in R (Version 0.1.0). Available from
#>   http://github.com/fabnavarro/rwavelet
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {R package {rwavelet}: Wavelet analysis in R},
#>     author = {F. Navarro and C. Chesneau},
#>     year = {2018},
#>     note = {(Version 0.1.0)},
#>     url = {http://github.com/fabnavarro/rwavelet},
#>   }
```
