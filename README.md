
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SciencesPo <img src="inst/figures/SciencesPo-logo.png" width="240px" align="right" />

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Build
Status](https://travis-ci.org/danielmarcelino/SciencesPo.svg?branch=master)](https://travis-ci.org/danielmarcelino/SciencesPo)
![CRAN Version](https://www.r-pkg.org/badges/version/SciencesPo)
![](https://img.shields.io/badge/license-GPL%20%28%3E=%202%29-blueviolet.svg?style=flat)
[![Coverage
Status](https://coveralls.io/repos/github/danielmarcelino/SciencesPo/badge.svg?branch=master)](https://coveralls.io/github/danielmarcelino/SciencesPo?branch=master)
[![Downloads](https://cranlogs.r-pkg.org/badges/SciencesPo)](https://cran.r-project.org/package=SciencesPo)
[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.54876.svg)](http://dx.doi.org/10.5281/zenodo.54876)

## A tool set for analyzing political science data

**SciencesPo** is a tool set for analyzing social and political behavior
data. It provides functions for analyzing elections results, including
measures of political fragmentation, seat apportionment, and a variety
of graph visualizations for small data.

## Installation

1 - From the CRAN repository:

``` r
  install.packages('SciencesPo', dep=TRUE)

  library(SciencesPo)
```

2 - To get the current development version from Github:

``` r
## install devtools package if it's not already
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

install_github("danielmarcelino/SciencesPo")

library(SciencesPo)
```

## Getting help

If you encounter a bug, please file a minimal reproducible example using
[reprex](https://reprex.tidyverse.org/index.html) or use [GitHub
issues](https://github.com/danielmarcelino/SciencesPo/issues). For
public questions and clarifications,
[StackOverflow](https://stackoverflow.com/) is a good place to ask.

## Helping out

**SciencesPo** is intended to be useful for the Social Sciences
community; contributions are very welcome\! Feel free to submit a [pull
request](https://github.com/danielmarcelino/SciencesPo/pulls).

## Usage

For a brief introduction to **SciencesPo** functionality, run:

``` r
demo(SciencesPo)
```

To see what functions are implemented in **SciencesPo**, run:

``` r
help(package=SciencesPo)
```

## Vignettes

  - [Concepts and Basics of
    SciencesPo](https://cran.r-project.org/web/packages/SciencesPo/vignettes/SciencesPo.html)
  - \[Descriptive Stats\]
  - \[Cross-Tabulation\]
  - [Measures of Political
    Behavior](https://cran.r-project.org/web/packages/SciencesPo/vignettes/Indices.html)
  - [The Anatomy of a
    Plot](https://cran.r-project.org/web/packages/SciencesPo/vignettes/Viz.html)
