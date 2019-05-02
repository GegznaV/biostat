
<!-- README.md is generated from README.Rmd. Please edit that file -->

-----

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/biostat)](https://cran.r-project.org/package=biostat)
[![GitHub
version](https://img.shields.io/badge/GitHub-v0.0.19-brightgreen.svg)](https://github.com/GegznaV/biostat)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/GegznaV/biostat?branch=master&svg=true)](https://ci.appveyor.com/project/GegznaV/biostat)
[![Travis-CI Build
Status](https://travis-ci.org/GegznaV/biostat.png?branch=master)](https://travis-ci.org/GegznaV/biostat)
[![codecov.io](https://codecov.io/github/GegznaV/biostat/coverage.svg?branch=master)](https://codecov.io/github/GegznaV/biostat?branch=master)
[![Updated-on](https://img.shields.io/badge/Updated%20on-2019--05--02-yellowgreen.svg)](/commits/master)

<!-- badges: end -->

-----

<img src="https://raw.githubusercontent.com/GegznaV/biostat/master/docs/logo.png" width="30%" height="30%" style="display: block; margin: auto;" />

# Package ***biostat*** – Routines for Basic (Bio)Statistics

Package ***biostat*** is an *R* package that contains a collection of
functions that either are intended to be used with R Commander plugin
*RcmdrPlugin.biostat* or to do several common statistical routines
(without writing to much code). The functions are created to complement
other *R Commander* plugins that can be used to teach basic statistics
in biostatistics and biometry lectures.

<font color="red"> The package is still in its **early development
stage** and some functions are for demonstration and test purposes only
as they may change in the future. </font>

Documentation and more information available at
<http://gegznav.github.io/biostat/>

<!-- ## Install package -->

<!-- To install a released version of the package from *CRAN*: -->

<!-- ```{r, eval=FALSE} -->

<!-- install.packages("biostat") -->

<!-- ``` -->

To install a developement version of the package from *GitHub*:

``` r
if (!"devtools" %in% installed.packages()) 
    install.packages("devtools")

devtools::install_github("GegznaV/biostat")
```

<!-- *** -->

## Related packages

<!-- Other related packages are *R Commander* (*Rcmdr*) plugins: -->

1.  **RcmdrPlugin.biostat** – an *R Commander* plugin for **biostat**
    package ([homepage](https://gegznav.github.io/RcmdrPlugin.biostat/))

<!-- b. **RcmdrPlugin.EZR.2** -- an *R Commander* plugin for the most common statistical analyses (the same as *RcmdrPlugin.EZR*, except that *RcmdrPlugin.EZR.2* does not modify original *Rcmdr* menu so dramatically); -->

<!-- c. **RcmdrPlugin.KMggplot2** -- an *R Commander* plugin for *ggplot2* graphics. -->

<!-- To install these packages, use the following code: -->

<!-- ```{r Install other packages, eval=FALSE} -->

<!-- # RcmdrPlugin.biostat -->

<!-- devtools::install_github("GegznaV/RcmdrPlugin.biostat") -->

<!-- # RcmdrPlugin.EZR.2 -->

<!-- devtools::install_github("GegznaV/RcmdrPlugin.EZR@unmodified_Rcmdr_menu") -->

<!-- # RcmdrPlugin.KMggplot2 -->

<!-- install.packages("RcmdrPlugin.KMggplot2") -->

<!-- ``` -->

<!--  <p align="right"> </p>     -->

-----
