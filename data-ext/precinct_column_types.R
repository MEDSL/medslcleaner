source('precinct_validity.R', local = TRUE)
precinct_column_types = sapply(precinct_validity, `[[`, 'type')
devtools::use_data(precinct_column_types , overwrite = TRUE)
