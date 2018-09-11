rwavelet
========

[![Build Status](https://travis-ci.org/fabnavarro/rwavelet.svg)](https://travis-ci.org/fabnavarro/rwavelet)

Wavelet Analysis

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

Here is an example of denoising of an experimental nuclear magnetic resonance (NMR) spectrum. We start by loading the data:

``` r
data("RaphNMR")
Y <- RaphNMR
n <- length(Y)
t <- seq(0, 1, length = n)
```

Then we specify the coarse decomposition scale *j*<sub>0</sub>, the wavelets we want to use (here, Symmlet with 6 null moments) and we perform a fast wavelet transform to get the noisy wavelet coefficients (Ywd):

``` r
j0 <- 0
J <- log2(n)
qmf <- MakeONFilter('Symmlet',6)
Ywd <- FWT_PO(Y, j0, qmf)
Ywnoise <- Ywd
```

We estimate the standard deviation *σ* of the noise using the maximum absolute deviation (with only the finest scale coefficients). We apply a hard thresholding rule (with a universal threshold) to the coefficient estimators and obtain the estimator by applying an inverse transform:

``` r
hatsigma <- MAD(Ywd[(2^(J-1)+1):2^J])
lambda <- sqrt(2*log(n))*hatsigma
Ywd[(2^(j0)+1):n] <- HardThresh(Ywd[(2^(j0)+1):n], lambda)
fhat <- IWT_PO(Ywd, j0, qmf)
```

Finally, we plot the resulting estimator:

``` r
par(mfrow=c(2,2), mgp = c(1.2, 0.5, 0), tcl = -0.2,
    mar = .1 + c(2.5,2.5,1,1), oma = c(0,0,0,0))
plot(t,Y,xlab="", ylab="", main="Observations")
plot(t,Y,xlab="", ylab="", main="Observations and Estimator")
matlines(t, fhat, lwd=2, col="blue", lty=1)
plot(Ywnoise, ylim=c(-20, 20), xlab="", ylab="", main = "Noisy Coefficients")
matlines(rep(lambda, n), lwd=2,col="red",lty=1)
matlines(-rep(lambda, n), lwd=2,col="red",lty=1)
plot(Ywd, ylim=c(-20,20), xlab="", ylab="", main = "Estimated Coefficients")
```

![](inst/doc/readme_img/NMR-1.png)

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
