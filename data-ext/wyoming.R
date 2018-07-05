#! /usr/bin/env Rscript
#
# Use final Wyoming precinct file as package data

library(data.table)
library(medslcleaner)

wyoming = fread('wyoming.csv')
wyoming = wyoming[, .(state_postal, year, office, district, stage, special,
                      jurisdiction, precinct, candidate, party, mode, writein,
                      votes, dataverse)]
wyoming[is.na(candidate) & writein, candidate := '[Write-in]']
wyoming = medslcleaner:::order_columns(wyoming)
wyoming = normalize_whitespace(wyoming)
devtools::use_data(wyoming, overwrite = TRUE)
