#' Save returns to disk after validation
#'
#' @param .data A table of returns.
#' @param state_postal State postal abbreviation.
#' @param skip Validations in \code{precinct_validity} to skip, as a list whose
#' names are variables and whose values are properties.
#' @import feather readr
#' @export
#' @examples
#' # 2016 MI precinct returns include statistical adjustments with negative
#' # `votes`, which requires an exception to the requirement that `votes` values
#' # are integers >= 0. We might pass:
#' skip = list(votes = 'is_valid')
#' # Because the `is_valid` callable in `precinct_validity$votes` enforces the
#' # >=0 requirement:
#' data(precinct_validity)
#' precinct_validity$votes
write_precincts = function(.data, state_postal, skip = NULL) {
  .data = rename_legacy_vars(.data)
  .data = fill_writein_candidate(.data)
  .data = keep_columns(.data)
  .data = assign_defaults(.data, state_postal)
  .data = normalize_whitespace(.data)
  .data = order_columns(.data)
  .data = convert_types(.data)
  assert_that(is_valid(.data, skip = skip))
  readr::write_csv(.data, glue("2016-{str_to_lower(state_postal)}-precinct.csv"))
  feather::write_feather(.data, glue("2016-{str_to_lower(state_postal)}-precinct.feather"))
}

#' Assign write-ins the `candidate` value of `['write-in']`
fill_writein_candidate = function(.data) {
  if ('writein' %in% colnames(.data)) {
    .data[is.na(candidate) & get('writein'), candidate := '[Write-in]']
  }
  .data[]
}

#' Convert types before saving
convert_types = function(.data) {
  # Type-convert character variables we expect to be integer/numeric/logical 
  for (varname in intersect(char_cols(.data), names(precinct_validity))) {
    if (precinct_validity[[varname]]$type %in% c('integer', 'numeric', 'logical')) {
      .data[, c(varname) := type.convert(get(varname), as.is = TRUE)]
    }
  }
  # Cast to integer any numeric variables we expect to be integer
  integer_columns = c('votes', 'year')
  for (varname in integer_columns) {
    if (has_name(.data, varname) && inherits(.data[[varname]], 'numeric')) {
      .data[, c(varname) := as.integer(get(varname))]
    }
  }
  .data[]
}

#' Assert validity of precinct returns
is_valid = function(.data, skip = NULL) {
  for (varname in names(precinct_validity)) {
    assert_that(has_name(.data, varname))
    assert_that(!anyDuplicated(.data))
    criteria = precinct_validity[[varname]]
    var_criteria_skipped = skip[[varname]]
    if (!'type' %in% var_criteria_skipped && !inherits(.data[[varname]], criteria$type)) {
      stop(glue('Expected {varname} to inherit from {criteria$type}; ',
        'got {class(.data[[varname]])}'))
    }
    if (!'no_na' %in% var_criteria_skipped && has_name(criteria, 'no_na') && isTRUE(criteria$no_na)) {
      if (!noNA(.data[[varname]])) {
        stop('Unexpected NAs in ', varname)
      }
    }
    if (!'n_unique' %in% var_criteria_skipped && has_name(criteria, 'n_unique') && criteria$n_unique > 0) {
      n_unique = length(unique(.data[[varname]]))
      if (n_unique != criteria$n_unique) {
        stop(glue('Expected {criteria$n_unique} unique values in {varname} ',
            'but got {n_unique}: {paste(unique(.data[[varname]]), collapse = ", ")}'))
      }
    }
    if (!'values' %in% var_criteria_skipped && has_name(criteria, 'values')) {
      values = unique(na.omit(.data[[varname]]))
      if (!all(values %in% criteria$values)) {
        unexpected_values = paste(setdiff(values, criteria$values), collapse = ', ')
        stop(glue('Unexpected values of {varname}: ', '{unexpected_values}'))
      }
    }
    if (!'is_valid' %in% var_criteria_skipped && has_name(criteria, 'is_valid')) {
      if (!all(criteria$is_valid(.data[[varname]]))) {
        stop(glue('Validation call unsuccessful for {varname}: ',
            '{paste(deparse(criteria$is_valid), collapse = "\\n")}'))
      }
    }
  }
  return(TRUE)
}

#' Keep expected columns in precinct returns
keep_columns = function(.data) {
  for (extra_col in setdiff(names(.data), names(precinct_validity))) {
    .data[, c(extra_col) := NULL]
  }
  .data = order_columns(.data)
  .data[]
}

#' Normalize whitespace in character columns
#'
#' Replaces 1+ whitespace characters with one space; trims leading and
#' trailing whitespace; and replaces zero-length strings with \code{NA}.
#'
#' @inheritParams write_precincts
#' @export
normalize_whitespace = function(.data, inner = TRUE) {
  columns = names(.data)[sapply(.data, is.character)]
  if (inner) {
    .data[, c(columns) := lapply(.SD, str_replace_all, '\\s+', ' '), .SDcols = columns]
  }
  .data[, c(columns) := lapply(.SD, str_trim), .SDcols = columns]
  for (column in columns) {
    .data[get(column) == '', c(column) := NA]
  }
  .data[]
}

#' Set column order in precinct returns
order_columns = function(.data) {
  col_order = intersect(names(.data), names(precinct_validity))
  setcolorder(.data, col_order)
  .data[]
}

#' Assign default values to missing precinct return variables
#' @export
assign_defaults = function(.data, state_postal) {
  if (!missing(state_postal) & !has_name(.data, 'state_postal')) {
    .data[, state_postal := str_to_upper(state_postal)]
  }
  if (!has_name(.data, 'writein')) {
    .data[, writein := FALSE]
  }
  if (!has_name(.data, 'year')) {
    .data[, year := 2016L]
  }
  if (!has_name(.data, 'stage')) {
    .data[, stage := "gen"]
  }
  if (!has_name(.data, 'special')) {
    .data[, special := FALSE]
  }
  if (!has_name(.data, 'mode')) {
    .data[, mode := 'total']
  }
  .data[]
}
