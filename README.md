<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/projmgr)](https://CRAN.R-project.org/package=projmgr)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis-CI Build Status](https://travis-ci.org/emilyriederer/projmgr.svg?branch=master)](https://travis-ci.org/emilyriederer/projmgr)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/emilyriederer/projmgr?branch=master&svg=true)](https://ci.appveyor.com/project/emilyriederer/projmgr)
[![Coverage status](https://codecov.io/gh/emilyriederer/projmgr/branch/master/graph/badge.svg)](https://codecov.io/github/emilyriederer/projmgr?branch=master)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/projmgr)](https://cran.rstudio.com/web/packages/projmgr/index.html)
<!-- badges: end -->


# projmgr <img src="man/figures/logo.png" align="right" height=140/>

`projmgr` aims to better integrate project management into your workflow and free up time for more exciting tasks like R coding and data analysis. Since many R users and programmers use GitHub as a home for their analysis, the goal of `projmgr` is to streamline project management with these same tools.

Key functionalities include:

- exchanging data with the GitHub API using user-friendly syntax
- generating issues and milestones from plain text (YAML) or R objects
- communicating (with reports or charts) project plans and progress to non-technical (non-GitHub using) collaborators

Just like communicating analytical results, good process communication is the key to success in most applied analysis settings. However, ad hoc processes or alternative tools can knock an analyst out of their R-based workflow and distract them from their core goals. 

## Try before you buy!

Want to find out more about `projmgr` before you install? Check out the [package website](https://emilyriederer.github.io/projmgr/) for an overview of features and example use cases.

## Installation

You can install `projmgr` on CRAN with:
```r
install.packages("projmgr")
```

Alternatively, you can install the most up-to-ddate development version of `projmgr` with:

``` r
devtools::install_github("emilyriederer/projmgr")
```

Please see the `NEWS.md` file or the [News page](https://emilyriederer.github.io/projmgr/news/index.html) on the website for details of major differences.

## Contributors

Thanks to contributors. Most notably, thanks to [Konrad Mayer](https://github.com/konradmayer) and [Sam Tyner](https://github.com/sctyner) for proactive reporting and debugging of issues.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.


