---
title: 'concstats:  Market Structure, Concentration and Inequality Measures'
tags:
  - R
  - market structure
  - concentration
  - inequality
  - diversity
authors:
  - name: Andreas Schneider
    orcid: 0000-0001-5630-1097
    affiliation: 1 # (Multiple affiliations must be quoted)
affiliations:
 - name: Independent Researcher, Schneider Risk & Business Consulting, Asuncion
   index: 1
citation_author: Schneider, A.
date: 17 May 2022
year: 2022
bibliography: paper.bib
output: rticles::joss_article
csl: apa.csl
journal: JOSS
---

# Summary

Measures of concentration, competition, and inequality are important key aspects
in many disciplines of social sciences. In economics they give a first insight of
a given market structure in a particular market. They are important to determine
public policies and strategic corporate decisions. However, in research and in
practice the most commonly used measure is the Herfindahl Hirschman Index
[@herfindahl:1950] [@hirschman:1945], calculated as the sum of squared shares
retained by individual firms in a given market.

# Statement of need

The Herfindahl Hirschman Index is still the most widely used measure when it
comes to determine concentration or diversity. In economics it usually defines 
if a given market is concentrated or not. However, relying on one single standard
measure may lead to erroneous conclusions and decisions. Various complementary or
alternative measures exist, which - used as a set - might reduce uncertainty
with respect to a given market situation, and, in consequence, make better and
informed decisions.
The goal of the `concstats` package is to offer a set of alternative and/or
additional measures. Various functions or groups of functions are available to
achieve the desired goal in a fraction of time.

The R package [@team:2000] `concstats` is designed to filling this gap, by
offering a simple-to-use set of 15 individual measures - more or less used by
academics or practitioners. 

# Overview and features

The package offers four different groups with the following functions or
functionality:

- **concstats** calculates eight pre-selected measures in a one-step procedure
to provide a first insight of the market.
- **mstruct** is a wrapper for the following market structure measures: **firm**,
**nrs_eq** (Numbers equivalent), **top**, **top 3** and **top 5** market share,
and **all** calculates all measures as a group.
- **comp** is a group wrapper for the following competition / concentration
measures: **hhi** [@herfindahl:1950] [@hirschman:1945], the **dual of the hhi**
[@tabak_et_al:2009], the **minimum of the hhi**, the **dominance index**
[@garcia:1994], the **stenbacka index** [@melnik_et_al:2008], and finally **all**
which calculates all measures as a group.
- **inequ** is a wrapper for inequality and diversity measures and contains:
**entropy** [@shannon:1948], **gini coefficient**, **simpson index**,
the **palma ratio** [@palma:2006] and the alternative **grs** measure
[@ginevicius:2009], **all** calculates the group measures.

For calculating any individual measure users just need to call the function,
e.g. hhi(), its dual hhi_d(), or any other function of interest, and include the
name of the vector with the market shares. The user may also calculate a group
of measures, e.g. the market structure, inequality measures, or even get a quick
overview of a given market calculating a predefined set of measures using the
concstats() function. 

Where it makes sense, measures are bounded [0, 1] to provide a better
comparability with respect to other measures. The user will be guided through a
number of defensive coding features avoiding erroneous inferences, e.g. a
warning message when the vector of shares is not in a decimal format or a stop
error message when the specified vector does not sum to 1. These and other
features should help the user to reduce uncertainty about a given market
situation and finally make better informed decisions in a consistent way and in
much shorter time.
A group of measures, e.g. `mstruct` (market structure) might be calculated in a
one-step procedure and will be printed in a table. Last but not least, the
package comes with a real world data set of large credit unions in Paraguay, used
in the article mentioned above.

A practical use case and further details regarding the different measures used
in this package can be found in the article
*Kreditgenossenschaften: Marktstruktur, Wettbewerb und Verhalten. Das Beispiel Paraguay*
[@Schneider:2022], which analyzes the market structure and competition of large
credit unions in Paraguay.
The package is available on CRAN and has a dedicated [website](https://schneiderpy.github.io/concstats/)
for a quick start.

# Examples

First of all, make sure you have the `concstats` package installed from CRAN or
the latest development version from Github.

A stable version of concstats is available on CRAN:

```r
install.packages("concstats") # Market structure, concentration and 
                              # inequality measures
```

You can install the development version from
[GitHub](https://github.com/schneiderpy/concstats) or:


```r
install.packages("devtools") # a package for developing R packages
devtools::install_github("schneiderpy/concstats")
```

Let us get a quick overview of a given market situation.


```r
library(concstats)
# We create some simple data
share <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
# market shares of each firm in the market (should sum up to 1)

# Calculate a selected set of market structure and concentration measures
share_con <- concstats(share) # creates the object share_con
#         Measures Values
# 1          Firms   6.00
# 2 Nrs_equivalent   3.33
# 3        Top (%)  40.00
# 4       Top3 (%)  85.00
# 5       Top5 (%)  96.00
# 6            HHI   0.30
# 7    Entropy(RE)   0.79
# 8    Palma ratio   2.67
```

Now, assume we are just interested in one single measure, e.g. the dominance
index.

```r
# We use the same vector, and we will name our object share_dom
share <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
share_dom <- concstats::dom(share)
share_dom

#> [1] 0.4519308

share <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
share_dom2 <- dom(share)
share_dom2

#> [1] 0.4519308

share <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
share_dom3 <- concstats::comp(share, type = "dom")
share_dom3

#> [1] 0.4519308

share <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
share_dom4 <- comp(share, type = "dom")
share_dom4

#> [1] 0.4519308
```
As you can see the functions can be accessed in different ways.
Now, let us assume we are interested in a group of measures, e.g. inequality.


```r
# Our known vector
share <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
share_ineq <- concstats::inequ(share, type = "all")
share_ineq

#         Measure Value
# 1       Entropy  2.04
# 2    Gini Index  0.46
# 3 Simpson Index  0.70
# 4   Palma Ratio  2.67
# 5           GRS  0.40
```

Note that for some measures the parameter unbiased = FALSE is the default setting.

# Visual summaries

The `concstats` package integrates fine with other visualization packages from
the R ecosystem, e.g. `kabelExtra` for tables or `ggplot2` for nice looking plots.

This time, we will use our ```creditcoops``` data set, which comes with the 
package.


```r
data("creditcoops")
head(creditcoops)

## A tibble: 6 x 5
## coop_id   year  total_loans paired total_loans_log
##  <dbl>    <fct>      <dbl>   <int>          <dbl>
##    1      2016    173892358    1            19.0
##    1      2018    199048199    1            19.1
##    2      2016    323892456    2            19.6
##    2      2018    461609439    2            20.0
##    3      2016    179981404    3            19.0
##    3      2018    227232008    3            19.2
```

You will need the following two packages. Make sure you have these packages
installed.

```r
library(dplyr)
library(kableExtra)
```

Now, we will filter out data for the year 2016.

```r
coops_2016 <- creditcoops %>% dplyr::filter(year == 2016)
head(coops_2016)

coops_2016 <-  coops_2016[["total_loans"]] # atomic vector of total loans
coops_2016 <- coops_2016/sum(coops_2016)   # convert the vector in decimal 
                                           # form of market shares

# We then use the new object `coops_2016` to calculate the market 
# structure measures as a group in a one-step-procedure:
coops_2016_mstruct <- mstruct(coops_2016, type = "all")
coops_2016_mstruct_tab <- coops_2016_mstruct %>% 
  kableExtra::kbl(caption = "Market structure 2016", digits = 2,
                  booktabs = T, align = "r") %>% 
  kableExtra::kable_classic(full_width = F, html_font = "Arial")
coops_2016_mstruct_tab
```
The result is a nice reusable table.


\includegraphics[width=5.24in]{mstruct_tab} 

Now, let's go a step further. We will make a visual comparison of the two
samples for years 2016 and 2018.
For this purpose, we will select from our `creditcoops` data set the relevant
columns (coop_id, year, paired, and total_loans_log) and make a new data frame.

Make sure you have the `ggplot2` package installed. Load the package.

```r
library(ggplot2) # Create Elegant Data Visualizations Using the Grammar
                 # of Graphics
```
<center>

![](coops_paired.png){width="469" height="521"}

</center>

Having a look a the output, we see a box plot with paired values of the
cooperatives and the evolution of their respective total loans over time for the
two sample years 2016 and 2018.

# References
