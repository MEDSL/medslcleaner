# Functions for working with Excel-formatted spreadsheets

# setDT data.frames that aren't already data.tables
to_datatable = function(.data) {
  if (!is.data.table(.data)) {
    setDT(.data)
  } else {
    .data
  }
}

#' Concatenate strings omitting NAs
#'
#' @inheritParams base::paste
#' @export
paste_omit_na = function(..., sep = " ", collapse = NULL) {
  values = na.omit(...)
  if (!length(values)) {
    NA
  } else {
    paste(values, sep = sep, collapse = collapse)
  }
}


#' Create identifier columns in spreadsheet data
#'
#' See the Excel vignette for use.
#'
#' @param .data A table of tidy spreadsheet data (i.e., with columns `row`, `col`, and `value`).
#' @param idcol String giving the name of the new identifier column.
#' @param rows One of: a vector of spreadsheet row indexes; a logical vector with
#'   length equal to the row count *of dataframe `.data`*; or a length-one logical
#'   vector (i.e., `TRUE` or `FALSE`) applied to all spreadsheet rows.
#' @param cols One of: a vector of spreadsheet column indexes; a logical vector with
#'   length equal to the column count *of dataframe `.data`*; or a length-one logical
#'   vector (i.e., `TRUE` or `FALSE`) applied to all spreadsheet columns
#' @param right If `TRUE`, carry non-missing values in cells selected by `rows` and
#'   cols` rightward along spreadsheet rows. Can be used together with `down`.
#' @param down If `TRUE`, carry non-missing values in cells selected by `rows` and
#'   cols` downward along spreadsheet rows. Can be used together with `up`.
#' @param .drop If `TRUE`, drop spreadsheet cells selected by `rows` and `cols` after
#'   creating the new identifier column.
#' @export
as_header = function(.data, idcol, rows = TRUE, cols = TRUE, right = FALSE, down =
  FALSE, .drop = TRUE) {
  setDT(.data, key = c('row', 'col'))
  # If rows gives row numbers, create an N-length logical vector, `TRUE` for rows
  # specified by `rows`
  if (is.numeric(rows)) {
    rows = .data$row %in% rows
  } else if (is.logical(rows)) {
    # Otherwise, if logical, it should be length-one or length-ncol(.data)
    assert_that(is.flag(rows) | length(rows) == nrow(.data))
  } else {
    stop(glue("rows can give row numbers, a logical vector with length equal to the ",
      "row count of .data ({nrow(.data)}), or be TRUE"))
  }
  # If cols gives column numbers, create an N-length logical vector, `TRUE` for
  # columns specified by `cols`
  if (is.numeric(cols)) {
    cols = .data$col %in% cols
  } else if (is.logical(cols)) {
    # Otherwise, if logical, it should be length-one or length-ncol(.data)
    assert_that(is.flag(cols) | length(cols) == nrow(.data))
  } else {
    stop(glue("cols can select columns by one of: their index; a logical vector with ",
      "length equal to the row count of .data ({nrow(.data)}), or a length-one ",
      "logical vector for all columns."))
  }
  # This works because .data is keyed
  .data[rows & cols, c(idcol) := value]
  # If `right` or `down`, carry forward non-missing `value` values, across rows
  # or down columns
  assert_that(is.flag(right))
  if (isTRUE(right)) {
    .data = carry_right(.data, value.name = idcol)
  }
  assert_that(is.flag(down))
  if (isTRUE(down)) {
    .data = carry_down(.data, value.name = idcol)
  }
  assert_that(is.flag(.drop))
  if (isTRUE(.drop)) {
    .data = .data[!(rows & cols)]
  }
  .data[]
}

#' Carry spreadsheet values forward row-wise or column-wise
#'
#' `carry_forward` implements row-wise or column-wise last non-missing
#' observation carried forward, in a table of tidy Excel data created by
#' [read_xlreturns()] or [tidyxl::xlsx_cells()]. Usually called by
#' [as_header()], not directly.
#'
#' @inheritParams as_header
#' @param value.name Column in `.data` to operate on.
#' @param .by Carry forward by `"row"` or by `"col"`.
#' @importFrom zoo na.locf0
#' @export
carry_forward = function(.data, rows = TRUE, cols = TRUE, .by = 'row',
  value.name = 'value') {
  .data = to_datatable(.data)
  assert_that(is.numeric(rows) || is.logical(rows))
  assert_that(is.numeric(cols) || is.logical(cols))
  assert_that(.data %has_name% 'row')
  assert_that(.data %has_name% 'col')
  assert_that(is.string(.by))
  assert_that(.by %in% c('row', 'col'))
  assert_that(.data %has_name% value.name)
  setkeyv(.data, c('row', 'col'))
  if (is.numeric(rows)) {
    rows = .data$row %in% rows
  }
  if (is.numeric(cols)) {
    cols = .data$col %in% cols
  }
  .data[rows & cols, c(value.name) := zoo::na.locf0(get(value.name)), by = .by][]
}

#' @rdname carry_forward
#' @export
carry_right = function(.data, rows = TRUE, cols = TRUE, value.name = 'value') {
  carry_forward(.data, rows = rows, cols = cols, .by = 'row', value.name =
    value.name)[]
}

#' @rdname carry_forward
#' @export
carry_down = function(.data, rows = TRUE, cols = TRUE, value.name = 'value') {
  carry_forward(.data, rows = rows, cols = cols, .by = 'col', value.name =
    value.name)[]
}

#' Split multiple tables in spreadsheets into separate dataframes
#'
#' @inheritParams as_header
#' @inheritParams stringr::regex
#' @param starts Whether the pattern indicates the start of a table or the end of the
#'   table. 
#' @export
split_cells = function(.data, pattern, rows = TRUE, cols = TRUE, starts = TRUE) {
  setDT(.data, key = c('row', 'col'))
  if (!'value' %in% names(.data)) {
    .data = combine_value_cols(.data)
  }
  if (is.numeric(rows)) {
    rows = .data$row %in% rows
  }
  if (is.numeric(cols)) {
    cols = .data$col %in% cols
  }
  rows = .data[rows & cols & str_detect(value, regex(pattern, TRUE)), sort(unique(row))]
  ranges = interval_to_index(rows, nrow(.data), starts)
  if (length(ranges)) {
    message(glue('Split sheet with {max(.data$row)} rows into {length(ranges)} tables'))
    for (r in ranges) {
      message(glue('  Rows {min(r)}-{max(r)}'))
    }
    return(lapply(ranges, function(rows_range) { .data[row %in% rows_range] }))
  } else {
    message('No match for `pattern`')
    return(list())
  }
}

interval_to_index = function(i, n = NULL, is_start = TRUE) {
  rows = as.integer(i)
  if (isTRUE(is_start)) {
    max_rows = ifelse(length(n), as.integer(n), max(rows))
    ends = purrr::imap_int(rows, ~ ifelse(.y == length(rows), max_rows, rows[.y + 1] - 1L))
    purrr::map2(rows, ends, ~ seq(.x, .y))
  } else {
    starts = purrr::imap_int(rows, ~ ifelse(.y == 1L, 1L, rows[.y - 1] + 1L))
    purrr::map2(starts, rows, ~ seq(.x, .y))
  }
}

#' Identify all-`NA` rows or columns in spreadsheet data
#'
#' @inheritParams as_header
#' @param .data A table of tidy spreadsheet data (i.e., with columns `row`, `col`, and `value`).
#' @param .by Either `"row"` to find all-NA spreadsheet rows or `"col"` to find all-NA
#'   columns.
#' @export
all_na = function(.data, .by = 'row', rows = TRUE, cols = TRUE) {
  if (!is.data.table(.data)) setDT(.data)
  stopifnot(.by %in% c('row', 'col'))
  if (is.numeric(rows)) {
    rows = .data$row %in% rows
  }
  if (is.numeric(cols)) {
    cols = .data$col %in% cols
  }
  na_rowcol = .data[rows & cols, all(is.na(value)), by = .by][
    (V1), c(.by), with = FALSE]
  .data[[.by]] %in% na_rowcol[[.by]]
}

#' Identify spreadsheet rows that contain a pattern
#'
#' @inheritParams as_header
#' @inheritParams stringr::regex
#' @export
row_contains = function(.data, pattern, rows = TRUE, cols = TRUE) {
  if (!is.data.table(.data)) setDT(.data)
  stopifnot('value' %in% names(.data))
  if (is.numeric(rows)) {
    rows = .data$row %in% rows
  }
  if (is.numeric(cols)) {
    cols = .data$col %in% cols
  }
  match_rows = .data[rows & cols, any(stringr::str_detect(value,
      stringr::regex(pattern, TRUE))), by = 'row'][(V1), row]
  .data$row %in% match_rows
}

#' @describeIn row_contains Identify spreadsheet columns that contain a pattern
#'
#' @export
#' @inheritParams row_contains
#' @inheritParams stringr::regex
col_contains = function(.data, pattern, rows = TRUE, cols = TRUE) {
  if (!is.data.table(.data)) setDT(.data)
  stopifnot('value' %in% names(.data))
  if (is.numeric(rows)) {
    rows = .data$row %in% rows
  }
  if (is.numeric(cols)) {
    cols = .data$col %in% cols
  }
  match_cols = .data[rows & cols, any(stringr::str_detect(value,
      stringr::regex(pattern, TRUE))), by = 'col'][(V1), row]
  .data$col %in% match_cols
}

#' Finalize spreadsheet data
#'
#' Drop columns in tidy spreadsheet data, and rename `values` as `votes`.
#'
#' @inheritParams write_precincts
#' @export
finalize = function(.data) {
  setDT(.data, key = c('row', 'col'))
  for (col in intersect(names(.data), c('sheet', 'address', 'row', 'col',
        'data_type', 'formula', "character", "character_formatted", "comment",
        "date", "error", "formula_group", "formula_ref", "height", "is_array",
        "is_blank", "local_format_id", "logical", "numeric",
        "style_format", "width"))) {
    .data[, (col) := NULL]
  }
  setnames(.data, 'value', 'votes')
  .data[, votes := type.convert(votes)]
  .data[]
}

totals = function(.data, .by = c('office', 'candidate')) {
  if (!is.data.table(.data)) setDT(.data)
  vote_col = ifelse('votes' %in% names(.data), 'votes', 'value')
  if (!is.numeric(.data[[vote_col]])) {
    message('Vote column not numeric')
    return(data.table())
  }
  .data[, sum(get(vote_col), na.rm = TRUE), by = .by][]
}

#' Combine value columns in spreadsheet data
#' 
#' Creates a new column, `value`, taking the values for each observation of
#' column `numeric`, `logical`, or `character`, whichever is specified in
#' `data_type`.
#' @inheritParams as_header
#' @export
combine_value_cols = function(.data) {
  # Combine value columns
  .data[data_type == 'numeric', value := as.character(numeric)]
  .data[data_type == 'logical', value := as.character(logical)]
  .data[data_type == 'character', value := character]
  .data[stringr::str_trim(value) == '', value := NA_character_]
  .data[]
}

#' Keep a subset of columns in spreadsheet data
#'
#' Keeps these columns, if they exist: `sheet`, `address`, `row`, `col`,
#' `data_type`, and `value`, and if `types` is `TRUE`, also `is_blank`,
#' `logical`, `numeric`, `date`, `character`, and `formula`.
#'
#' @inheritParams as_header
#' @param types If `TRUE`, also keep columns `is_blank`, `logical`, `numeric`,
#'   `date`, `character`, and `formula`.
#' @export
#' @examples
#' .data = data.frame(row=rep(1:3, each = 2), col = rep(1:2, times = 3), value = sample.int(6),
#'   format_code = sample.int(6))
#' head(.data)
#' keep_minimal(.data)
keep_minimal = function(.data, types = FALSE) {
  if (!is.data.table(.data)) setDT(.data)
  cols = c("sheet", "address", "row", "col", "data_type", "value")
  if (types) {
    cols = c(cols, c("is_blank", "logical", "numeric", "date", "character", "formula"))
  }
  # Drop columns not in cols
  drop_cols = setdiff(names(.data), cols)
  for (col in drop_cols) {
    .data[, c(col) := NULL]
  }
  .data[]
}

#' Retrieve paths to packaged Excel spreadsheets
#'
#' The package includes spreadsheets for use in examples and vignettes.
#' Installation places them in the package directory. This function retrieves
#' the path to a spreadsheet, given one of the names below.
#'
#' Spreadsheet names: 
#' * `common-toc`
#' * `louisiana`
#' * `merrimack-excerpt`
#' * `nebraska`
#' * `new-hampshire`
#' * `new-york-cortland`
#' * `ohio`
#' * `oregon`
#' * `south-dakota`
#' * `utah-salt-lake`
#' * `vermont`
#' * `wisconsin`
#'
#' @param name Name of an Excel file example. See details.
#' @return Path to the example.
#' @export
#' @examples
#' # Paths will vary system-to-system
#' spreadsheet_example('merrimack-excerpt')
spreadsheet_example = function(name) {
  assert_that(is.string(name))
  path = system.file(glue('{name}.xlsx'), package = 'medslcleaner')
  if (!file.exists(path)) {
    stop(deparse(name), ' is not the name of an example spreadsheet')
  }
  path
}

