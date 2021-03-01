
<!-- README.md is generated from README.Rmd. Please edit that file -->

# typogenerator: Several Mechanisms for Typo Generation

<!-- badges: start -->

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://choosealicense.com/licenses/apache-2.0/)
![CRAN status](https://www.r-pkg.org/badges/version/typogenerator)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![codecov](https://codecov.io/gh/mikemahoney218/typogenerator/branch/master/graph/badge.svg)](https://codecov.io/gh/mikemahoney218/typogenerator)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R build
status](https://github.com/mikemahoney218/typogenerator/workflows/R-CMD-check/badge.svg)](https://github.com/mikemahoney218/typogenerator/actions)

<!-- badges: end -->

## Overview

This package is an R implementation of the Golang
[Typogenerator](https://github.com/zntrio/typogenerator) library,
incorporating several methods for creating typos from a set of input
strings.

All of these methods work on character vectors of any length and have a
standard interface via the `typo_*` functions:

``` r
library(typogenerator)

typo_hyphenation("lip")
#> $lip
#> [1] "-lip" "l-ip" "li-p" "lip-"

typo_transposition(c("cats", "dogs"))
#> $cats
#> [1] "acts" "ctas" "cast"
#> 
#> $dogs
#> [1] "odgs" "dgos" "dosg"
```

In addition, generating typos via multiple methods can be done easily
using the `generate_typos` function:

``` r
generate_typos("snow", c("typo_addition", "typo_prefix"))
#> $snow
#>  [1] "snow-" "snow." "snow0" "snow1" "snow2" "snow3" "snow4" "snow5" "snow6"
#> [10] "snow7" "snow8" "snow9" "snow_" "snowa" "snowb" "snowc" "snowd" "snowe"
#> [19] "snowf" "snowg" "snowh" "snowi" "snowj" "snowk" "snowl" "snowm" "snown"
#> [28] "snowo" "snowp" "snowq" "snowr" "snows" "snowt" "snowu" "snowv" "snoww"
#> [37] "snowx" "snowy" "snowz" "-snow" ".snow" "0snow" "1snow" "2snow" "3snow"
#> [46] "4snow" "5snow" "6snow" "7snow" "8snow" "9snow" "_snow" "asnow" "bsnow"
#> [55] "csnow" "dsnow" "esnow" "fsnow" "gsnow" "hsnow" "isnow" "jsnow" "ksnow"
#> [64] "lsnow" "msnow" "nsnow" "osnow" "psnow" "qsnow" "rsnow" "ssnow" "tsnow"
#> [73] "usnow" "vsnow" "wsnow" "xsnow" "ysnow" "zsnow"
```

By default, these functions will only use characters already inside of
the word or which are generally accepted as file names on English-based
systems, but users can define their own character options by passing
vectors to the `chars` arguments of each function.

Note that this package is in very early development and the user-facing
API is liable to change.

## Installation

You can install the development version of terrainr from
[GitHub](https://github.com/mikemahoney218/typogenerator) with:

``` r
# install.packages("devtools")
devtools::install_github("mikemahoney218/typogenerator")
```
