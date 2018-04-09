library(data.table)
virginia_precincts = fread('virginia_precincts.csv')
devtools::use_data(virginia_precincts, overwrite = TRUE)
