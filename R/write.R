#' Save returns to disk
#'
#' Write data to disk as `.csv` and `.Rds`, using the naming convention
#' `2016-{state_postal}-precinct`. Transformations are applied before saving;
#' see details.
#'
#' Before saving:
#'
#' * Rename or drop legacy variables, which appear in some old intermediate
#' CSVs, e.g., `total votes` or `candidate.votes`
#' * Keep only the columns defined in the `[fields]` schema
#' * Assign default values for these columns (e.g., `FALSE` for `special`)
#' * Normalize whitespace in character columns (trim exterior whitespace;
#'     replace 1+ interior whitespace with single whitespace)
#' * Reorder the columns
#' * Convert variables to expected types (e.g., numeric -> integer), which
#'   should be non-destructive if the `validate(.data)` was successful.
#'
#' @param .data A table of returns.
#' @param state_postal State postal abbreviation.
#' @return Processed `.data`.
#' @export
write_precincts = function(.data, state_postal) {
  if (!is.data.table(.data)) setDT(.data)
  .data = rename_legacy_vars(.data)
  .data = keep_columns(.data)
  .data = assign_defaults(.data, state_postal)
  .data = normalize_whitespace(.data)
  .data = order_columns(.data)
  fwrite(.data, glue("2016-{str_to_lower(state_postal)}-precinct.csv"))
  saveRDS(.data, glue("2016-{str_to_lower(state_postal)}-precinct.Rds"))
  .data[]
}

# Rename legacy variable names and drop unused legacy variables
rename_legacy_vars = function(.data) {
  if (!is.data.table(.data)) setDT(.data)
  if (has_name(.data, 'candidate.votes')) {
    setnames(.data, 'candidate.votes', 'votes')
  }
  if (has_name(.data, 'candidatevotes')) {
    setnames(.data, 'candidatevotes', 'votes')
  }
  if (has_name(.data, 'write.in')) {
    setnames(.data, 'write.in', 'writein')
  }
  if (has_name(.data, 'total.votes')) {
    .data[, total.votes := NULL]
  }
  .data = normalize_state(.data)
  .data[]
}


# Keep expected columns in precinct returns
keep_columns = function(.data) {
  data('fields', package = 'medslcleaner', envir = environment())
  if (!is.data.table(.data)) setDT(.data)
  for (extra_col in setdiff(names(.data), names(fields))) {
    .data[, c(extra_col) := NULL]
  }
  .data = order_columns(.data)
  .data[]
}

# Set column order in precinct returns
order_columns = function(.data) {
  if (!is.data.table(.data)) setDT(.data)
  data('fields', package = 'medslcleaner', envir = environment())
  col_order = intersect(names(.data), names(fields))
  setcolorder(.data, col_order)
  .data[]
}

#' Assign default values to missing precinct return variables
#' 
#' Create and fill with reasonable defaults the variables `writein`, `year`,
#' `stage`, `special`, `mode`, and `candidate` where `writein` is `TRUE`.
#' 
#' @inheritParams write_precincts
#' @export
assign_defaults = function(.data, state_postal) {
  if (!is.data.table(.data)) setDT(.data)
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
  if ('writein' %in% colnames(.data)) {
    .data[is.na(candidate) & get('writein'), candidate := '[Write-in]']
  }
  .data[]
}
