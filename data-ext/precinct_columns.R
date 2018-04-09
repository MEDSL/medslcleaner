source('precinct_validity.R', local = TRUE)
precinct_columns = names(precinct_validity)
devtools::use_data(precinct_columns, overwrite = TRUE)
