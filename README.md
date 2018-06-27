
<!-- README.md is generated from README.Rmd. Please edit that file -->

# medslcleaner

The `medslcleaner` R package helps prepare election returns for release
by [MEDSL](https://electionlab.mit.edu). We use the package in R scripts
that normalize or clean raw data. It’s in early development and anything
may change.

## Installation

Install from GitHub using e.g. `devtools`.

``` r
if (!require(devtools, quietly = TRUE)) install.packages('devtools')
devtools::install_github('MEDSL/medslcleaner')
```

## Getting started

  - [Read about our workflow for cleaning elections
    data](https://github.com/MEDSL/medslcleaner/blob/master/vignettes/workflow.md)
  - [Learn how to extract data from spreadsheets quickly and
    safely](https://github.com/MEDSL/medslcleaner/blob/master/vignettes/excel.md)
