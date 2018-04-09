#! /usr/bin/env Rscript
#
#  Create example returns dataset

library(data.table)
library(readxl)

merrimack = fread('../data-ext/example-returns/Merrimack-NH-2016_edits.csv',
  header = FALSE)
devtools::use_data(merrimack, overwrite = TRUE)
