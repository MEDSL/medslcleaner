#! /usr/bin/env Rscript
#
# Use raw Virginia precinct file as package data

library(data.table)

virginia = fread('virginia.csv')
devtools::use_data(virginia, overwrite = TRUE)
