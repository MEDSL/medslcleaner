#! /usr/bin/env Rscript

library(data.table)
library(stringr)
library(yaml)
library(assertthat)

field_schema = yaml.load_file('schema/fields.yaml')
fields = field_schema$fields
field_names = sapply(fields, `[[`, 'name')
fields = lapply(fields, function(x) {
  assert_that(x %has_name% 'type')
  if (is.null(x$constraints)) {
    x$constraints = list()
  }
  if (is.null(x$constraints$required)) {
    x$constraints$required = FALSE
  }
  if (!is.null(x$required)) {
    x$required = NULL
  }
  x
})
fields = setNames(fields, field_names)
devtools::use_data(fields, overwrite = TRUE)

wyoming = fread('../../2016-precinct-data/data/WY/final/2016-wy-precinct.csv')
devtools::use_data(wyoming, overwrite = TRUE)

