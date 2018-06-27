# Functions for working with Excel-formatted spreadsheets

#' Read Excel spreadsheets
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

#' Carry values right in tidy Excel data
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

#' Carry values right in tidy Excel data
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

#' Create identifier columns in tidy Excel data
#' 
#' See the Excel vignette for use.
#' 
#' @export
as_idcol = function(.data, idcol, i = TRUE, j = TRUE, right = FALSE, down =
  FALSE, .drop = TRUE) {
  setDT(.data, key = c('row', 'col'))
  if (is.numeric(i)) {
    i = .data$row %in% i
  }
  if (is.numeric(j)) {
    j = .data$col %in% j
  }
  .data[i & j, c(idcol) := value]
  if (isTRUE(right)) {
    .data = carry_right(.data, idcol = idcol)
  }
  if (isTRUE(down)) {
    .data = carry_down(.data, idcol = idcol)
  }
  if (isTRUE(.drop)) {
    .data = .data[!(i & j)]
  }
  .data[]
}

#' Subset columns in a tidyxl table to those typically useful
#' 
#' @inheritParams as_idcol
#' @export
keep_minimal = function(.data, types = FALSE) {
  setDT(.data, key = c('row', 'col'))
  cols = c("sheet", "address", "row", "col", "data_type", "value")
  if (types) {
    cols = c(cols, c("is_blank", "logical", "numeric", "date", "character", "formula"))
  }
  .data = .data[, c(cols), with = FALSE]
  .data[]
}

#' Collapse rows of characters
#'
#' Concatenate rows column-wise into a single row.
#'
#' @param .data A data.table.
#' @param i A vector of row indexes.
#' @param f Concatenation (or other) function.
#' @param drop Drop all but the first row.
#' @export
collapse_rows = function(.data, i, f = paste_omit_na, collapse = ' ', drop = TRUE)  {
  if (!is.data.table(.data)) setDT(.data)
  char_cols = names(.data)[sapply(.data, is.character)]
  if (length(char_cols) != length(.data)) {
    warning('Dropping non-character values in i')
  }
  if (length(char_cols)) {
    .data[i, (char_cols) := lapply(.SD, f, collapse = collapse), .SDcols = char_cols]
    .data = .data[-i[-1]]
  }
  if (isTRUE(drop)) {
    .data = .data[-c(setdiff(i, i[1]))]
  }
  .data
}

#' Collapse cells
#'
#' Concatenate rows column-wise into a single row.
#'
#' @param .data A data.table.
#' @param i A vector of row indexes.
#' @param f Concatenation (or other) function.
#' @param drop Drop all but the first row.
#' @export
collapse_cells = function(.data, i, f = paste_omit_na, collapse = ' ', .drop = TRUE) {
  if (!is.data.table(.data)) setDT(.data)
  stopifnot('row' %in% colnames(.data) & 'value' %in% colnames(.data))
  if (!'value' %in% names(.data)) {
    .data = combine_value_cols(.data)
  }
  if (is.numeric(i)) {
    i = .data$row %in% i
  }
  # if (is.numeric(j)) {
  #   j = .data$col %in% j
  # }
  if (sum(i) <= 0) {
    stop("Need more than 1 row to collapse")
  }
  .data[i, value := f(value, collapse = collapse)]
  .data = .data[-i[-1]]
  if (isTRUE(.drop)) {
    .data = .data[!(i)]
  }
  .data
}

#' Expand headers to include values in initial rows
#' 
#' @export
#' @examples
#' d = data.table(A = c('B', 'C'), L = c('M', 'N'), stringsAsFactors = FALSE)
#' d = expand_colnames(d, 1:2)
#' d
#> Empty data.table (0 rows) of 2 cols: A B C,L M N
#' d = data.table(A = c('B', 'C'), L = c('M', 'N'), stringsAsFactors = FALSE)
#' d = expand_colnames(d, 2)
#' d
#>    A C L N
#> 1:   B   M
expand_colnames = function(.data, i, f = paste_omit_na, collapse = ' ', drop = TRUE) {
  if (!is.data.table(.data)) setDT(.data)
  suffixes = vapply(.data[i, ], f, collapse = collapse, FUN.VALUE = character(1))
  setnames(.data, colnames(.data), paste(colnames(.data), na.omit(suffixes), sep = collapse))
  if (drop) {
    .data = .data[-i, ]
  }
  .data[]
}

#' Split tables by pattern
#'
#' Search for headers that delimit multiple tables in a spreadsheet, and read
#' the tables separately.
#' @importFrom purrr imap map_lgl
#' @export
split_sheet = function(path, pattern_column, pattern, adjust_start = 0,
  adjust_max = 0, sheet = NULL, idcol = NULL, use_names = TRUE) {

  # First read the full sheet that we'll search for table headers
  full = readxl::read_excel(path, col_names = FALSE, sheet = sheet)
  setDT(full)
  # Identify the rows in which headers appear
  header_rows = which(full[[pattern_column]] %=% pattern)
  # Get the ranges of each table given the header indexes
  ranges = interval_to_index(header_rows, nrow(full), is_start = FALSE)
  ranges = setNames(ranges, full[[pattern_column]][header_rows])

  # Read each table in the full sheet independently
  tables = purrr::imap(ranges, function(.range, id) {
    .skip = max(0, min(.range) - 1 + adjust_start)
    .n_max = length(.range) + adjust_max
    cat('skip: ', .skip, '\n')
    cat('n_max: ', .n_max, '\n')
    tbl = readxl::read_excel(path, skip = .skip, n_max = .n_max, sheet = sheet,
        col_names = TRUE)
    # Drop all-NA columns
    suppressWarnings({
      tbl[, names(tbl)[purrr::map_lgl(tbl, ~ all(is.na(.x) | str_trim(.x) == ''))] := NULL]
    })
    # Assign the value of the district pattern match to a new district column
    if (length(idcol)) {
      tbl[, c(idcol) := id]
    }
    tbl
  })
  if (use_names) {
    tables
  } else {
    unname(tables)
  }
}

#' Split cells by row
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

#' Combine the results of split_sheet()
#' 
#' Deprecated in favor of [finalize]
#' 
#' @export
combine_sheet = function(returns, id.vars = 'precinct') {
  returns = suppressWarnings({lapply(returns, melt, id.vars = id.vars,
    value.name = 'votes', variable.name = 'candidate', variable.factor =
      FALSE)})
  returns = rbindlist(returns, fill = TRUE)
  returns = returns[!votes %=% '-']
  returns[, votes := type.convert(votes)]
  returns[, L1 := NULL]
  returns = expand_parties(returns)
  returns[]
}

#' Predicate for all-`NA` rows or columns in tidy excel tables
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

#' Predicate for rows in tidy excel table containing a pattern
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

#' Drop columns in tidyxl data not used in final returns data
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

#' Drop tidyxl rows via predicate functions
#'
#' Example uses are dropping spreadsheet rows beyond some (spreadsheet) row
#' number, or dropping spreadsheet rows in which any cell's values match
#' a pattern like \code{'total'}.
#'
#' @export
#' @examples
#' library(data.table)
#' d = data.table(row = 1:4, character = c('a', 'b', 'c', 'total'))
#' d
#' drop_predicates = list(
#'   function(d) { d$row == 3 },
#'   function(d) { d$character %=% 'total' })
#' # predicate 1 -> drop rows 3
#' # predicate 2 -> drop 'total' row
#' drop_by_predicate(d, drop_predicates)
drop_by_predicate = function(d, drop_predicates) {
  if (!is.data.table(d)) setDT(d)
  assert(is.list(drop_predicates))
  assert(has_name(d, 'row'))
  # Get the values of matching *row columns* in a tidyxl table
  drop_rows = map(drop_predicates, ~ d[.x(d), row])
  # Drop all rows whose *row column* value was matched
  drop_rows = unique(unlist(drop_rows))
  d[!row %in% drop_rows][]

}

#' Combine value columns in tidyxl data
#' 
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
