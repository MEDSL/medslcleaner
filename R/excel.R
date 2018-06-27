# Functions for working with Excel-formatted spreadsheets

#' Read spreadsheet data from Excel files
#'
#' @export
#' @examples
#' d = read_xlreturns('../data-ext/example-returns/16gen_stwd_pct.xlsx')
#' as_idcol(d, 'office', i = 1)
read_xlreturns = function(path = path, ..., keep_all = FALSE) {
  ret = do.call(tidyxl::xlsx_cells, c(path = path, list(...)))
  ret = combine_value_cols(setDT(ret))
  if (!isTRUE(keep_all)) {
    ret = keep_minimal(ret)
  }
  ret[]
}

#' Concatenate strings omitting NAs
#'
#' @export
paste_omit_na = function(..., sep = " ", collapse = NULL) {
  values = na.omit(...)
  if (!length(values)) {
    NA
  } else {
    paste(values, sep = sep, collapse = collapse)
  }
}

#' Carry spreadsheet values right row-wise
#'
#' Usually called by [as_idcol], not directly.
#'
#' @importFrom zoo na.locf0
#' @export
carry_right = function(.data, i = TRUE, idcol = NULL) {
  setDT(.data, key = c('row', 'col'))
  stopifnot('value' %in% names(.data))
  if (is.numeric(i)) {
    i = .data$row %in% i
  }
  if (length(idcol)) {
    .data[i, c(idcol) := zoo::na.locf0(get(idcol)), by = 'row'][]
  } else {
    .data[i, value := zoo::na.locf0(value), by = 'row'][]
  }
  .data[]
}

#' Carry spreadsheet values down column-wise
#'
#' Usually called by [as_idcol], not directly. This is row-wise last
#' non-missing observation carried forward.
#'
#' @inheritParams as_idcol
#' @export
carry_down = function(.data, i = TRUE, j = TRUE, idcol = NULL) {
  setDT(.data, key = c('row', 'col'))
  stopifnot('value' %in% names(.data))
  if (is.numeric(i)) {
    i = .data$row %in% i
  }
  if (is.numeric(j)) {
    j = .data$col %in% j
  }
  if (length(idcol)) {
    .data[i & j, c(idcol) := zoo::na.locf0(get(idcol)), by = 'col'][]
  } else {
    .data[i & j, value := zoo::na.locf0(value), by = 'col'][]
  }
}

#' Create identifier columns in spreadsheet data
#'
#' See the Excel vignette for use.
#'
#' @param .data A table of tidy spreadsheet data (i.e., with columns `row`, `col`, and `value`).
#' @param idcol String giving the name of the new identifier column.
#' @param i One of: a vector of spreadsheet row indexes; a logical vector with
#'   length equal to the row count of dataframe .data; or a length-one logical
#'   vector applied to all spreadsheet rows."))
#' @param i One of: a vector of spreadsheet column indexes; a logical vector with
#'   length equal to the *row* count of dataframe .data; or a length-one logical
#'   vector applied to all spreadsheet columns."))
#' @param right If `TRUE`, carry non-missing values in cells selected by `i` and
#'   j` rightward along spreadsheet rows. Can be used together with `down`.
#' @param down If `TRUE`, carry non-missing values in cells selected by `i` and
#'   j` downward along spreadsheet rows. Can be used together with `up`.
#' @param .drop If `TRUE`, drop spreadsheet cells selected by `i` and `j` after
#'   creating the new identifier column.
#' @export
as_idcol = function(.data, idcol, i = TRUE, j = TRUE, right = FALSE, down =
  FALSE, .drop = TRUE) {
  setDT(.data, key = c('row', 'col'))
  # If i gives row numbers, create an N-length logical vector, `TRUE` for rows
  # specified by `i`
  if (is.numeric(i)) {
    i = .data$row %in% i
  } else if (is.logical(i)) {
    # Otherwise, if logical, it should be length-one or length-ncol(.data)
    assert_that(is.flag(i) | length(i) == nrow(.data))
  } else {
    stop(glue("i can give row numbers, a logical vector with length equal to the ",
      "row count of .data ({nrow(.data)}), or be TRUE"))
  }
  # If j gives column numbers, create an N-length logical vector, `TRUE` for
  # columns specified by `j`
  if (is.numeric(j)) {
    j = .data$col %in% j
  } else if (is.logical(j)) {
    # Otherwise, if logical, it should be length-one or length-ncol(.data)
    assert_that(is.flag(j) | length(j) == nrow(.data))
  } else {
    stop(glue("j can select columns by one of: their index; a logical vector with ",
      "length equal to the row count of .data ({nrow(.data)}), or a length-one ",
      "logical vector for all columns."))
  }
  # This works because .data is keyed
  .data[i & j, c(idcol) := value]
  # If `right` or `down`, carry forward non-missing `value` values, across rows
  # or down columns
  assert(is.flag(right))
  if (isTRUE(right)) {
    .data = carry_right(.data, idcol = idcol)
  }
  assert(is.flag(down))
  if (isTRUE(down)) {
    .data = carry_down(.data, idcol = idcol)
  }
  assert(is.flag(.drop))
  if (isTRUE(.drop)) {
    .data = .data[!(i & j)]
  }
  .data[]
}

#' Keep a subset of columns in spreadsheet data
#'
#' Keeps these columns, if they exist: `sheet`, `address`, `row`, `col`,
#' `data_type`, and `value`, and if `types` is `TRUE`, also `is_blank`,
#' `logical`, `numeric`, `date`, `character`, and `formula`.
#'
#' @inheritParams as_idcol
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

#' Split multiple tables in spreadsheets into separate dataframes
#'
#' @export
split_cells = function(.data, pattern, i = TRUE, j = TRUE, starts = TRUE) {
  setDT(.data, key = c('row', 'col'))
  if (!'value' %in% names(.data)) {
    .data = combine_value_cols(.data)
  }
  if (is.numeric(i)) {
    i = .data$row %in% i
  }
  if (is.numeric(j)) {
    j = .data$col %in% j
  }
  i = .data[i & j & str_detect(value, regex(pattern, TRUE)), sort(unique(row))]
  ranges = interval_to_index(i, nrow(.data), starts)
  if (length(ranges)) {
    message(glue('Split sheet with {max(.data$row)} rows into {length(ranges)} tables'))
    for (r in ranges) {
      message(glue('  Rows {min(r)}-{max(r)}'))
    }
    return(lapply(ranges, function(i_range) { .data[row %in% i_range] }))
  } else {
    message('No match for `pattern`')
    return(list())
  }
}

# TODO: need same but for footers, i.e., starting range from 1 and ending with
# first index; etc.
#
# If row 2 gives 'District 1' and row 29 gives 'District 2', then read rows 3-28
# and then rows 30+
#' @importFrom purrr imap_int pmap
row_ranges = function(rows, n_rows, from_first = FALSE) {
  end_rows = purrr::imap_int(rows, ~ if (is.na(rows[.y + 1])) {
    n_rows
  } else {
    rows[.y + 1] - 1L - .x - 1L
  })
  purrr::pmap(list(rows, end_rows), function(.start, .end) c(.start, .end))
}

interval_to_index = function(i, n = NULL, is_start = TRUE) {
  i = as.integer(i)
  if (isTRUE(is_start)) {
    max_i = ifelse(length(n), as.integer(n), max(i))
    ends = purrr::imap_int(i, ~ ifelse(.y == length(i), max_i, i[.y + 1] - 1L))
    purrr::map2(i, ends, ~ seq(.x, .y))
  } else {
    starts = purrr::imap_int(i, ~ ifelse(.y == 1L, 1L, i[.y - 1] + 1L))
    purrr::map2(starts, i, ~ seq(.x, .y))
  }
}

#' Identify all-`NA` rows or columns in spreadsheet
#'
#' @export
all_na = function(.data, .by = 'row', i = TRUE, j = TRUE) {
  if (!is.data.table(.data)) setDT(.data)
  stopifnot(.by %in% c('row', 'col'))
  if (is.numeric(i)) {
    i = .data$row %in% i
  }
  if (is.numeric(j)) {
    j = .data$col %in% j
  }
  na_ij = .data[i & j, all(is.na(value)), by = .by][
    (V1), c(.by), with = FALSE]
  .data[[.by]] %in% na_ij[[.by]]
}

#' Identify spreadsheet rows that contain a pattern
#'
#' @export
row_contains = function(.data, pattern, i = TRUE, j = TRUE) {
  if (!is.data.table(.data)) setDT(.data)
  stopifnot('value' %in% names(.data))
  if (is.numeric(i)) {
    i = .data$row %in% i
  }
  if (is.numeric(j)) {
    j = .data$col %in% j
  }
  match_i = .data[i & j, any(stringr::str_detect(value,
      stringr::regex(pattern, TRUE))), by = 'row'][(V1), row]
  .data$row %in% match_i
}

#' @describeIn row_contains Identify spreadsheet columns that contain a pattern
#'
#' @export
#' @inheritParams row_contains
col_contains = function(.data, pattern, i = TRUE, j = TRUE) {
  if (!is.data.table(.data)) setDT(.data)
  stopifnot('value' %in% names(.data))
  if (is.numeric(i)) {
    i = .data$row %in% i
  }
  if (is.numeric(j)) {
    j = .data$col %in% j
  }
  match_j = .data[i & j, any(stringr::str_detect(value,
      stringr::regex(pattern, TRUE))), by = 'col'][(V1), row]
  .data$col %in% match_j
}

#' Finalize spreadsheet data
#'
#' Drop columns in tidy spreadsheet data, and rename `values` as `votes`.
#'
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

# Combine value columns in tidyxl data
combine_value_cols = function(.data) {
  # Combine value columns
  .data[data_type == 'numeric', value := as.character(numeric)]
  .data[data_type == 'logical', value := as.character(logical)]
  .data[data_type == 'character', value := character]
  .data[stringr::str_trim(value) == '', value := NA_character_]
  .data[]
}

by_row = function(.data) {
  .data %>%
    group_by(row, col)
}

by_column = function(.data) {
  .data %>%
    group_by(col, row)
}
