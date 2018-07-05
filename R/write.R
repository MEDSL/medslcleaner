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

#' Normalize whitespace in character columns
#'
#' Replaces 1+ whitespace characters with one space; trims leading and
#' trailing whitespace; and replaces zero-length strings with \code{NA}.
#'
#' @inheritParams write_precincts
#' @param inner Whether to normalize inner whitespace.
#' @export
normalize_whitespace = function(.data, inner = TRUE) {
  if (!is.data.table(.data)) setDT(.data)
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
