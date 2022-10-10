---
editor_options: 
  markdown: 
    wrap: 72
---

# Contributing to the concstats R package

Your want to contribute to **concstats**: Market Structure,
Concentration and Inequality Measures? Your are welcome!

For more detailed info about how you can contribute to this R package,
please be so kind and read the following:

## Understanding the scope of concstats

`concstats` solves a very specific set of problems focused on the
measurement of market structure, concentration, and inequality. The need
for those measures are often (but not limited) inherent in areas like
economics, public policies, marketing or even monitoring institutions of
national market competition. When it comes to make informed decisions
and a single standard measure may lead to erroneous conclusions, the
`concstats` package may offer alternative measures or even a set of
measures for this task. By itself it is not intended as a replacement
for packages that create publication ready tables or plots, however, it
works fine with standard visualization packages. The basic concept is
that of calculating a single or a set of more or less frequently used
measures for market structure, concentration, and inequality in a fast
and flexible way to make informed decisions in research and practice.

### Fixing, changing, adding, asking

#### Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the
documentation directly using the GitHub web interface, as long as the
changes are made in the *source* file. This generally means you'll need
to edit [roxygen2
comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in the `.R`
script, not a `.Rd` file. You can find the `.R` file that generates the
`.Rd` by reading the comment in the first line.

#### Fixing bugs

Please submit questions, bug reports, and requests in the [issues
tracker](https://github.com/schneiderpy/concstats/issues). Please submit
bug reports with a minimal
[reprex](https://www.tidyverse.org/help/#reprex) (including details like
operating system and local setup).

#### Bigger changes

If you want to make a bigger change, it's a good idea to first file an
[issue](https://github.com/schneiderpy/concstats/issues) and discuss
your idea.

-   I aim for testing that has high coverage and is robust. Include
    tests with any major contribution to code. Test your changes to the
    package with `testthat`, `goodpractice` and `devtools::check()` (aim
    for 0 errors and warnings) before submitting your change.

-   New code should follow the tidyverse [style
    guide](https://style.tidyverse.org). You can use the
    [styler](https://CRAN.R-project.org/package=styler) package to apply
    these styles, but please don't restyle code that has nothing to do
    with your PR.

-   I'll generally include contributors as authors in the DESCRIPTION
    file (with their permission) for most contributions that go beyond
    small typos in code or documentation.

### What's next?

`constats`has reached a stable state of development, with active
subsequent development primarily in response to user feedback.

#### Still got questions?

Using **concstats** and got stuck? Browse the
[documentation](https://schneiderpy.github.io/concstats) to see if you
can find a solution. Still stuck? Post your question as an [issue on
GitHub](https://github.com/schneiderpy/concstats/issues/new). While I
cannot offer user support, I'll try to do my best to address it, as
questions often lead to better documentation or the discovery of bugs.

Want to ask a question in private? Contact the package maintainer by
[email](mailto:schneiderconsultingpy@gmail.com).

### Propose an idea?

Have an idea for a new concstats feature? Take a look at the
[documentation](https://schneiderpy.github.io/concstats) and [issue
list](https://github.com/schneiderpy/concstats/issues) to see if it
isn't included or suggested yet. If not, suggest your idea as an [issue
on GitHub](https://github.com/schneiderpy/concstats/issues/new). While I
can't promise to implement your idea, it helps to:

-   Explain in detail how it would work.

-   Keep the scope as narrow as possible.

## Citing concstats

You like the idea of the package? You will use `concstats` in research
or practice in the future?\
Please consider [citing
it](https://schneiderpy.github.io/concstats/authors.html)!

## Code of Conduct

When contributing to `concstats` you must follow the [code of conduct
defined by rOpenSci](https://ropensci.org/code-of-conduct/). By
contributing to this project you agree to abide by its terms.
