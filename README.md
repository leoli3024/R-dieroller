
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Overview

The package `"dieroller"` is a minimal implementation for simulating rolling
a die a given number of times, and to visualize the relative frequencies
in such a series of rolls.

  - `die()` creates a die object (of class `"die"`)
  - `roll()` rolls a die object, producing a `"roll"` object.
  - `plot()` method for a `"roll"` object to plot relative frequencies of sides.
  - `summary()` method for a `"roll"` object.

## Motivation

This package has been developed to illustrate some of the concepts
behind the creation of an R package.

## Installation

Install the development version from GitHub via the package
`"devtools"`:

``` r
# development version from GitHub:
#install.packages("devtools") 

# install "cointoss" (without vignettes)
devtools::install_github("leoli3024/dieroller")

# install "cointoss" (with vignettes)
devtools::install_github("leoli3024/dieroller", build_vignettes = TRUE)
```

## Usage

``` r
library(dieroller)

# default coin
fair_die <- die()
fair_die

# 1 toss of coin1
roll(fair_die)

# 10 tosses of coin1
roll10 <- roll(fair_die, times = 10)
roll10

# summary
summary(roll10)

# 100 tosses
roll100 <- roll(fair_die, times = 100)

# summary
summary(roll100)
```
