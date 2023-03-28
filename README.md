
<!-- README.md is generated from README.Rmd. Please edit that file -->

# concstats <img src='man/figures/logo.png' align="right" height="150" style="float:right; height:150px;"/>

# Market Structure, Concentration, and Inequality Measures

## Introduction

<!-- badges: start -->

[![R-CMD-check](https://github.com/ropensci/concstats/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci/concstats/actions)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/concstats)](https://cran.r-project.org/package=concstats)
[![runiverse](https://ropensci.r-universe.dev/badges/concstats)](https://ropensci.r-universe.dev/concstats)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![PRsWelcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=plastic)](https://github.com/ropensci/concstats/pulls)
[![R
badge](https://img.shields.io/badge/Build%20with-♥%20and%20R-blue)](https://github.com/ropensci/concstats)
[![codecov](https://codecov.io/gh/ropensci/concstats/branch/master/graph/badge.svg?token=IG5NPEGK6J)](https://app.codecov.io/gh/ropensci/concstats)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6456536.svg)](https://doi.org/10.5281/zenodo.6456536)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/concstats)](https://cran.r-project.org/package=concstats)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/concstats?color=blue)](https://r-pkg.org/pkg/concstats)
[![pkgcheck](https://github.com/ropensci/concstats/workflows/pkgcheck/badge.svg)](https://github.com/ropensci/concstats/actions?query=workflow%3Apkgcheck)
[![Status at rOpenSci Software Peer
Review](https://badges.ropensci.org/559_status.svg)](https://github.com/ropensci/software-review/issues/559)
<!-- badges: end -->

Measures of concentration and competition are important and give a first
insight of a given market structure in a particular market. They are
important to determine public policies and strategic corporate
decisions. However, in research and in practice the most commonly used
measure is the Herfindahl Hirschman Index. Various complementary or
alternative measures exist, which - used as a set - might reduce
uncertainty. The goal of the
[concstats](https://cran.r-project.org/package=concstats) package is to
offer a set of alternative and/or additional measures for researchers in
social sciences and practitioners in institutions concerned with
competition on a regular basis to better determine a given market
structure and therefore reduce uncertainty with respect to a given
market situation. Various functions or groups of functions are available
to achieve the desired goal.

\-`concstats_concstats` calculates a set of pre-selected concentration
and diversity measures in a one-step procedure.  
-`concstats_mstruct` offers market structure measures, e.g. the sum of
Top3 or Top5 market shares.  
-`concstats_comp` is a wrapper for concentration measures, e.g. the
Herfindahl Hirschman Index.  
-`concstats_inequ` offers diversity or inequality measures, e.g. the
Entropy or the Palma ratio.

## Installation

A stable version of concstats is available on CRAN:

``` r
install.packages("concstats") # Market structure, concentration and inequality
                              # measures
```

You can install the latest development version from
[GitHub](https://github.com/ropensci/concstats) or
[R-universe](https://ropensci.r-universe.dev/ui#package:concstats).

``` r
# install.packages("devtools")
devtools::install_github("ropensci/concstats")
```

``` r
install.packages("concstats", repos = "https://ropensci.r-universe.dev")
```

## How to use `concstats`

### `concstats_concstats`

In general, `concstats` takes numeric vectors as input, that is,
relative market shares in decimal format. `concstats_constats` has one
main function which calculates a set of pre-selected measures in a
one-step procedure.

### `concstats_mstruct`

is a wrapper to calculate different structural measures. Within this
group are measures like the number of firms, numbers equivalent,
cumulative Top 3 and Top 5 market share. The measures might be
calculated as a group or individually.

### `concstats_comp`

is a group wrapper to calculate different concentration measures. Within
this group are measures like the Herfindahl-Hirschman index (HHI), the
dual of the HHI, the Dominance or the Stenbacka index.

### `concstats_inequ`

is a group of inequality and diversity measures, e.g. Entropy, Gini
coefficient, Palma ratio. Most functions offer a finite sample
correction.

## Examples

This is a basic example which shows you how to calculate an individual
measure or a set of market structure and concentration measures:

``` r
library(concstats)
## Create some simple data
x <- c(0.4, 0.2, 0.25, 0.1, 0.05, 0, 0)
concstats_hhi(x) # the Herfindahl-Hirschman Index
#> [1] 0.275

concstats_dom(x) # the Dominance Index
#> [1] 0.4127273

## Our simple data
x2 <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04) # market shares of each firm in
                                          # the market (should sum up to 1)

## Calculate a selected set of market structure and concentration measures
concstats_concstats(x2, digits = 2) # calculates a selected set of measures
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
Herfindahl-Hirschman Index, in decimal form, 7) Normalized Shannon
Entropy (RE), a value between 0 and 1, 8) Palma ratio, an inequality
score which measures the ratio of the top 10 percent to the bottom 40
percent.

## *Prior Art*

Some functions are already implemented in other R packages. The
non-exhaustive summary below is by no means a description of each
package.

-   The Herfindahl Hirschman Index can be found in the
    [hhi](https://joss.theoj.org/papers/10.21105/joss.00828) and the
    [divseg](https://github.com/christopherkenny/divseg) packages. While
    the *hhi* package has just one function, neither of both packages
    offer a normalized version of the measure.

-   The latter offers as well functions for the Entropy, Gini and
    Simpson measures.

-   The [acid](https://cran.r-project.org/package=acid) and the
    [ineq](https://cran.r-project.org/package=ineq) packages offer
    functions for inequality and competition measures, e.g. for the
    Entropy and Gini metric.

Some popular measures, e.g. Gini or the Herfindahl-Hirschman index have
also been implemented in Python.

However, almost none of these packages offer a normalized calculation of
the respective measure, with the exception of the
[ineq](https://cran.r-project.org/package=ineq) package. Other functions
are new implementations in R, e.g. Dominance Index, Palma ratio,
Stenbacka Index, GRS measure, and the dual of the Herfindahl Hirschman
Index.

## References

1.  Chang, E. J., Guerra, S. M., de Souza Penaloza, R. A. & Tabak, B. M.
    2005) “Banking concentration: the Brazilian case” In Financial
          Stability Report. Brasilia: Banco Central do Brasil, 4:
          109-129.
2.  Cobham, A. and A. Summer (2013). “Is It All About the Tails? The
    Palma Measure of Income Inequality”, Center for Global Development,
    Washington, DC.
3.  Garcia Alba Idunate, P. (1994). “Un Indice de dominancia para el
    analisis de la estructura de los mercados”. , 61: 499-524.
4.  Ginevicius, R. and S. Cirba (2009). “Additive measurement of market
    concentration”, , 10(3), 191-198.
    <https://doi.org/10.3846/1611-1699.2009.10.191-198>
5.  Palma, J. G. (2006). “Globalizing Inequality: ‘Centrifugal’ and
    ‘Centripetal’ Forces at Work”, DESA Working Paper No. 35.
6.  Shannon, C. E. (1948). “A Mathematical Theory of Communication”,
    (Nokia Bell Labs).
7.  Melnik, A., Shy, Oz, Stenbacka, R., (2008), “Assessing market
    dominance”, , 68: pp. 63-72.
8.  Simpson, E. H. (1949). “Measurement of Diversity”, , 163, 688.
    <https://doi.org/10.1038/163688a0>

## Credits

The hexagon sticker is created by myself with the `hexsticker` package.
A good overview and a lot of inspiration (adding badges, how to create a
webpage and testing the package) comes from [Cosima Meyer and Dennis
Hammerschmidt](https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/r-package/).

## Contact and Issues

If you have any questions or find any bugs requiring fixing, feel free
to open an issue or pull request.

## Development

Contributions are welcome! For more details on how to contribute to this
package please see the [CONTRIBUTING
file](https://docs.ropensci.org/concstats/CONTRIBUTING.html).

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.
