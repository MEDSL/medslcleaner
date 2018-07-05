#' Validate election returns against schema
#'
#' @param .data A table of election returns.
#' @export
validate = function(.data) {
  message('Validating:')
  data('fields', package = 'medslcleaner', envir = environment())
  for (field_name in names(fields)) {
    validate_field(.data, field_name)
  }
  message('Success!')
}

#' @describeIn validate Validate a single field
#' @inheritParams validate
#' @param name The name of a column in `.data`.
#' @export
validate_field = function(.data, name) {
  field = field_schema(name)
  # Variable must exist
  assert_that(.data %has_name% name)
  # Variable must have expected type
  if (field$type == 'string') {
    assert_that(has_string_values(.data, name))
  } else if (field$type == 'boolean') {
    assert_that(has_logical_values(.data, name))
  } else if (field$type == 'integer') {
    assert_that(has_integer_values(.data, name))
  } else if (field$type == 'numeric') {
    assert_that(is.numeric(.data[[name]]))
  }
  # If values are enumerated, values must be among those numerated
  enum = field[['constraints']][['enum']]
  if (!is.null(enum)) {
    assert_that(is_enumerated(.data, name, enum = enum))
  }
  # If values are required, none can be missing
  is_required = field[['constraints']][['required']]
  if (!is.null(is_required) && is_required) {
    assert_that(none_missing(.data, name))
  }
  message(glue(' ✔ {name}'))
  TRUE
}

#' Retrieve field schema for a single field
#'
#' @param name The name of a field.
#' @export
field_schema = function(name) {
  data(fields, package = 'medslcleaner', envir = environment())
  assert_that(in_schema(name))
  field = fields[[name]]
  field
}

#' @describeIn validate Select non-integer values
#' @export
select_noninteger = function(.data, name) {
  assert_that(.data %has_name% name)
  if (!is.numeric(.data[[name]])) {
    selection = .data
    message(glue('"{name}" is {class(.data[[name]])}, not numeric or integer'))
  } else {
    if (!is.data.table(.data)) setDT(.data)
    selection = .data[((get(name) %% 1) != 0) & !is.na(get(name))]
  }
  message(glue('{nrow(selection)}/{nrow(.data)} rows take non-integer ',
      '"{name}" values'))
  selection[]
}

#' @describeIn validate Select non-string values
#' @export
select_nonstring = function(.data, name) {
  assert_that(.data %has_name% name)
  if (!is.data.table(.data)) setDT(.data)
  if (!is.character(.data[[name]])) {
    selection = .data
    message(glue('"{name}" is factor, not character'))
  } else {
    selection = .data[str_trim(get(name)) == '' & !is.na(get(name))]
    message(glue('{nrow(selection)}/{nrow(.data)} rows take non-string ',
        '"{name}" values'))
  }
  selection[]
}

#' @describeIn validate Select non-enumerated values
#' @export
select_not_enumerated = function(.data, name) {
  # Retrieve enumerated values
  field = field_schema(name)
  enum = field[['constraints']][['enum']]
  if (is.null(enum)) {
    stop(glue('Schema for {name} does not specify enumerated possible values'))
  }
  # Select all other values
  assert_that(.data %has_name% name)
  .data = to_datatable(.data)
  selection = .data[!get(name) %in% enum][]
  message(glue('{nrow(selection)}/{nrow(.data)} rows take "{name}" values ',
      'not enumerated in schema'))
  selection[]
}

#' @describeIn validate Select missing values
#' @export
select_missing = function(.data, name) {
  assert_that(.data %has_name% name)
  .data = to_datatable(.data)
  selection = .data[is_missing(get(name))]
  message(glue('{nrow(selection)}/{nrow(.data)} rows have missing "{name}" ',
      'values '))
  selection[]
}

is_enumerated = function(.data, name, enum) {
  all(.data[[name]] %in% enum)
}
on_failure(is_enumerated) <- function(call, env) {
  paste0("Not all values of ", eval(call$name, env), " are enumerated possible values")
}

has_string_values = function(.data, name) {
  is.character(.data[[name]]) && !any(str_trim(na.omit(.data[[name]])) == '')
}
on_failure(has_string_values) <- function(call, env) {
  paste0("Values of ", eval(call$name, env), " should be string")
}

has_logical_values = function(.data, name) {
  is.logical(.data[[name]])
}
on_failure(has_logical_values) <- function(call, env) {
  paste0("Values of ", eval(call$name, env), " should be logical")
}

in_schema = function(name) {
  data(fields, package = 'medslcleaner', envir = environment())
  assert_that(is.string(name))
  name %in% names(fields)
}

on_failure(in_schema) = function(call, env) {
  paste0(eval(call$name, env), " is not a schema-defined field")
}

has_integer_values = function(.data, name) {
  is.numeric(.data[[name]]) && all((na.omit(.data[[name]]) %% 1) == 0)
}

on_failure(has_integer_values) <- function(call, env) {
  paste0("Values of ", eval(call$name, env), " should be integer")
}

is_missing = function(x) {
  if (is.character(x) | is.factor(x)) {
    is.na(x) | str_trim(x) == ''
  } else {
    is.na(x)
  }
}

none_missing = function(.data, name) {
  !any(is_missing(.data[[name]]))
}

on_failure(none_missing) <- function(call, env) {
  paste0(eval(call$name, env), " has missing values.")
}

