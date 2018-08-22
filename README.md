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

See the [package vignette](http://fnavarro.perso.math.cnrs.fr/rpackage/rwaveletvignette.html) for details. You could also build and see the vignette associated with the package using the following lines of code

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
