[![R-CMD-check](https://github.com/KWB-R/kwb.smartwater/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KWB-R/kwb.smartwater/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/KWB-R/kwb.smartwater/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.smartwater/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.smartwater/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.smartwater)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.smartwater)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.smartwater)](https://kwb-r.r-universe.dev/)

# kwb.smartwater

This package contains functions related to the SmartWater
project.

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'kwb.smartwater' from GitHub
remotes::install_github("KWB-R/kwb.smartwater")
```

## Documentation

Release: [https://kwb-r.github.io/kwb.smartwater](https://kwb-r.github.io/kwb.smartwater)

Development: [https://kwb-r.github.io/kwb.smartwater/dev](https://kwb-r.github.io/kwb.smartwater/dev)
