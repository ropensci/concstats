
# concstats <img src='man/figures/logo.png' align="right" height="150" style="float:right; height:150px;"/>

# Market Structure, Concentration and Inequality Measures

## Introduction

<!-- badges: start -->

[![R-CMD-check](https://github.com/schneiderpy/concstats/workflows/R-CMD-check/badge.svg)](https://github.com/schneiderpy/concstats/actions)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/concstats)](https://cran.r-project.org/package=concstats)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![PRs
Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=plastic)](https://github.com/schneiderpy/concstats/pulls)
[![R
badge](https://img.shields.io/badge/Build%20with-♥%20and%20R-blue)](https://github.com/schneiderpy/concstats)
[![Codecov test
coverage](https://codecov.io/gh/schneiderpy/concstats/branch/master/graph/badge.svg)](https://app.codecov.io/gh/schneiderpy/concstats?branch=master)
[![DOI](https://zenodo.org/badge/332882922.svg)](https://zenodo.org/badge/latestdoi/332882922)
<!-- [![metacran downloads](https://cranlogs.r-pkg.org/badges/concstats)](https://cran.r-project.org/package=concstats) -->
<!-- [![cran checks](https://cranchecks.info/badges/summary/concstats)](https://cran.r-project.org/web/checks/check_results_concstats.html) -->
<!-- [![](https://cranlogs.r-pkg.org/badges/version/concstats)](https://www.r-pkg.org/badges/version/concstats) -->
<!-- badges: end -->

Measures of concentration and competition are important and give a first
insight of a given market structure in a particular market. They are
important to determine public policies and strategic corporate
decisions. However, in research and in practice the most commonly used
measure is the Herfindahl Hirschman Index. The goal of the `concstats`
package is to offer a set of alternative and/or additional measures to
better determine a given market structure and therefore reduce
uncertainty with respect to a given market situation. Various functions
or groups of functions are available to achieve the desired goal.

\-`concstats` calculates a set of pre-selected concentration and
diversity measures in a one-step procedure.  
-`mstruct` offers market structure measures, e.g. the sum of Top3 or
Top5 market shares.  
-`comp` is a wrapper for concentration measures, e.g. the Herfindahl
Hirschman Index.  
-`inequ` offers diversity or inequality measures, e.g. the Entropy or
the Palma ratio.

## Installation

A stable version of concstats is available on CRAN:

``` r
install.packages("concstats") # Market structure, concentration and inequality measures
library(concstats)
```

You can install the development version from
[GitHub](https://github.com/schneiderpy/concstats) or:

``` r
install.packages("devtools") # a package for developing R packages
devtools::install_github("schneiderpy/concstats")
```

## How to use `concstats`

### `concstats`

has one main function which calculates a set of pre-selected measures in
a one-step procedure.

### `mstruct`

is a wrapper to calculate different structural measures. Within this
group are measures like the number of firms, numbers equivalent,
cumulative Top 3 and Top 5 market share. The measures might be
calculated as a group or individually.

### `comp`

is a group wrapper to calculate different concentration measures. Within
this group are measures like the Herfindahl-Hirschman index (HHI), the
dual of the HHI, the Dominance or the Stenbacka index.

### `inequ`

is a group of inequality and diversity measures, e.g. Entropy, Gini
coefficient, Palma ratio.

## Examples

This is a basic example which shows you how to calculate an individual
measure or a set of market structure and concentration measures:

``` r
library(concstats)
## Create some simple data
share1 <- c(0.4, 0.2, 0.25, 0.1, 0.05, 0, 0)
share_hhi <- hhi(share1) # the Herfindahl-Hirschman Index
#> [1] 0.275

share_dom <- dom(share1) # the Dominance Index
#> [1] 0.4127273

## Our simple data
share2 <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04) # market shares of each firm in
                                              # the market (should sum up to 1)

## Calculate a selected set of market structure and concentration measures
share2_con <- concstats(share2) # creates share2_con, a selected set of measures
#>         Measures Values
#> 1          Firms   6.00
#> 2 Nrs_equivalent   3.33
#> 3        Top (%)  40.00
#> 4       Top3 (%)  85.00
#> 5       Top5 (%)  96.00
#> 6            HHI   0.30
#> 7    Entropy(RE)   0.79
#> 8    Palma ratio   2.67
```

In this case, the result is a table with eight selected measures: 1)
Number of firms, 2) Numbers equivalent of firms, 3) Top firm, share in
%, 4) Top 3 firms, share in %, 5) Top 5 firms, share in %, 6) The
Herfindahl-Hirschman Index, in decimal form, 7) Normalized Entropy (RE),
a value between 0 and 1, 8) Palma ratio, an inequality score which
measures the ratio of the top 10 percent to the bottom 40 percent.

## References

1.  Chang, E. J., Guerra, S. M., de Souza Penaloza, R. A. & Tabak, B. M.
    2005) Banking concentration: the Brazilian case. In Financial
          Stability Report. Brasilia: Banco Central do Brasil, 4:
          109-129.
2.  Cobham, A. and A. Summer (2013). Is It All About the Tails? The
    Palma Measure of Income Inequality, Center for Global Development,
    Washington, DC.
3.  Garcia Alba Idunate, P. (1994). Un Indice de dominacion para el
    analisis de la estructura de los mercados. El Trimestre Economico,
    61: 499-524.
4.  Ginevicius, R. and S. Cirba (2009). Additive measurement of market
    concentration, Journal of Business Economics and Management, 10(3),
    191-198.
5.  Palma, J. G. (2006). Globalizing Inequality: ‘Centrifugal’ and
    ‘Centripetal’ Forces at Work, DESA Working Paper No. 35.
6.  Shannon, C. E. (1948). A Mathematical Theory of Communication, The
    Bell System Technical Journal (Nokia Bell Labs).

## Credits

The hexagon sticker is created by myself with the `hexsticker` package.
A good overview and a lot of inspiration (adding badges, how to create a
webpage and testing the package) comes from [Cosima Meyer and Dennis
Hammerschmidt](https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/r-package/).

## Contact and Issues

If you have any questions or find any bugs requiring fixing, feel free
to open an issue or pull request.

## Development

Contributions are welcome!
