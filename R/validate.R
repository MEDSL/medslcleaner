#' Compare vote totals
#'
#' Compare vote totals with a reference dataset. By default, the reference
#' totals are the [state_totals] dataset from [Dave Leip's Atlas of U.S.
#' Presidential Elections](https://uselectionatlas.org), for `candidate` values
#' of Donald Trump, Hillary Clinton, Gary Johnson, and "Total" (the sum of votes
#' for these three and all other candidates).
#'
#' We sum `votes` in each dataframe by the variables specified in argument `by`,
#' and then left-join `.data` and the reference dataframe on the same variables.
#'
#' @param .data Precinct returns dataframe.
#' @param reference A dataframe of reference vote totals. See details.
#' @param by A vector of shared column names in `.data` and `reference` to merge
#'   on.
#' @param all.y Perform a full join, instead of a left join. This will include
#'   in the result all rows from the reference dataset.
#' @return A copy of `.data` with state-candidate vote totals and their
#'   difference from the reference totals.
#' @export
#' @examples
#' # Use default reference totals from https://uselectionatlas.org
#' .data = data.frame(office = 'US President', candidate = c('Gary Johnson',
#'   'Donald Trump'), state_postal = 'AL', votes = c(44000, 1318200))
#' compare_votes(.data)
#'
#' # Use an arbitrary dataframe for reference totals
#' reference = data.frame(office = 'US President', candidate = c('Gary Johnson',
#'   'Donald Trump'), state_postal = 'AL', votes = c(43022, 1327200))
#' compare_votes(.data, reference)
compare_votes = function(.data, reference = NULL, by = c('state_postal',
    'office', 'candidate'), all.y = FALSE) {
  if (!is.data.table(.data)) setDT(.data)
  stopifnot(all(c(by, 'votes') %in% names(.data)))
  if (!is.null(reference)) {
    if (!is.data.table(reference)) setDT(reference)
    reference = copy(reference)
    stopifnot(all(c(by, 'votes') %in% names(reference)))
    reference = reference[office == 'US President', .(votes = sum(votes, na.rm =
        TRUE)), by = by]
    state_totals = reference
  } else {
    data('state_totals', envir = environment())
    setDT(state_totals)
    state_totals[, `:=`(office = 'US President', state = NULL)]
  } 
  .data = copy(.data)[, .(votes = sum(votes, na.rm = TRUE)), by = by] 
  .data = merge(.data, state_totals, all.x = TRUE, all.y = all.y, by = by,
    suffixes = c("", "_reference"))
  .data[, diff_reference := votes - votes_reference]
  .data[]
}

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
  assert_that(fields %has_name% name)
  field = fields[[name]]
  field
}

has_integer_values = function(.data, name) {
  is.numeric(.data[[name]]) && all((na.omit(.data[[name]]) %% 1) == 0)
}
on_failure(has_integer_values) <- function(call, env) {
  paste0("Values of ", eval(call$name, env), " should be integer")
}

#' @describeIn validate Select non-integer values
#' @export
select_noninteger = function(.data, name) {
  assert_that(.data %has_name% name)
  if (!is.numeric(.data[[name]])) {
    selection = .data
  } else {
    if (!is.data.table(.data)) setDT(.data)
    selection = .data[((get(name) %% 1) != 0) & !is.na(get(name))]
  }
  message(glue('{nrow(selection)}/{nrow(.data)} rows take non-integer ',
      '"{name}" values'))
  selection[]
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

#' @describeIn validate Select non-string values
#' @export
select_nonstring = function(.data, name) {
  assert_that(.data %has_name% name)
  if (!is.data.table(.data)) setDT(.data)
  if (!is.character(.data[[name]])) {
    selection = .data
  } else {
    selection = .data[str_trim(get(name)) == '' & !is.na(get(name))]
  }
  message(glue('{nrow(selection)}/{nrow(.data)} rows take non-string ',
      '"{name}" values'))
  selection[]
}

is_enumerated = function(.data, name, enum) {
  all(.data[[name]] %in% enum)
}
on_failure(is_enumerated) <- function(call, env) {
  paste0("Not all values of ", eval(call$name, env), " are enumerated possible values")
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

#' @describeIn validate Select missing values
#' @export
select_missing = function(.data, name) {
  assert_that(.data %has_name% name)
  .data = to_datatable(.data)
  selection = .data[is_missing(get(name))]
  message(glue('{nrow(selection)}/{nrow(.data)} rows have missing "{name}" ',
      ' values '))
  selection[]
}

