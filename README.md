rwavelet
========

Wavelet Transforms in R

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

See the [package vignette](http://fnavarro.perso.math.cnrs.fr/rpackage/rwaveletvignette.html) for details. You could also build and see the vignette associated with the package using the following lines of code

``` r
devtools::install_github("fabnavarro/rwavelet", build_vignettes = TRUE)
library(rwavelet)
```

Then, to view the vignette

``` r
vignette("rwaveletvignette")
```
