concstats: Calculate market structure and concentration measures
================
Andreas Schneider

<!-- README.md is generated from README.Rmd. Please edit that file -->

## Introduction

<!-- badges: start -->

\[![R build
status](https://github.com/schneiderpy/concstats/workflows/R-CMD-check/badge.svg)\]
[![PRs
Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=plastic)](https://github.com/schneiderpy/concstats/pulls)
<!-- badges: end -->

Measures of concentration and competition are important and give a first
insight of a given market structure and conduct in a particular market.
They are important to determine public policies. However, in research
and in practice the most widely used measure is the Herfindahl-Hirschman
Index or better known as HHI. The goal of `concstats` is to offer a set
of alternative and/or additional measures to better determine a given
market structure.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("schneiderpy/concstats")
```

## How to use `concstats`

`concstats`has one main function which calculates all the measures in
one step. This is a basic example which shows you how to calculate a set
of market structure and concentration measures:

``` r
library(concstats)
## Create some simple data
share <- c(0.4, 0.266, 0.334, 0, 0) # market share of each firm in the market (should sum up to 1)

## Calculate market structure and concentration measures
con_share <- concstats(share) # creates con_share
con_share # view con_share in a table format
```

| measures            | values |
| :------------------ | -----: |
| Firms               |   3.00 |
| Nrs equivalant      |   2.92 |
| Top (%)             |  40.00 |
| Top3 (%)            | 100.00 |
| Top5 (%)            | 100.00 |
| HHI                 |   0.34 |
| HHI(min)            |   0.33 |
| HHI(dual)           |   0.03 |
| Dominance index     |   0.37 |
| Stenbacka index (%) |  47.58 |
| RE                  |   0.99 |
| Berry index         |   0.66 |

The result is a table with 12 measures: 1) Number of firms, 2) Numbers
equivalant of firms, 3) Top firm, share in %, 4) Top 3 firms, share in
%, 5) Top 5 firms, share in %, 6) The Herfindahl-Hirschman Index (HHI),
in decimal form, 7) The min. of HHI, in decimal form, 8) The dual of
HHI, in decimal form, 9) Dominance index, a value between 0 and 1, 10)
Stenbacka index, in %, a threshold which may indicates a dominant
position, 11) Normalized Entropy (RE), a value between 0 and 1, 12)
Berry index (or Simpson index), a value between 0 and 1

## Contact and Issues

If you have any questions or find any bugs requiring fixing, feel free
to open an issue or pull request.
