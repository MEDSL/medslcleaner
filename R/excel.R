# Functions for working with Excel-formatted spreadsheets

#' Read Excel spreadsheets
#'
#' @export
#' @examples
#' `%>%` = magrittr::`%>%`
#' d = read_xlreturns('../data-ext/example-returns/16gen_stwd_pct.xlsx')
#' d = d %>%
#'   as_idcol('office', i = 1)
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
  cols = c("sheet", "address", "row", "col", "data_type", "formula", "value")
  if (types) {
    cols = c(cols, c("is_blank", "logical", "numeric", "date", "character"))
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
  stopifnot(is.data.table(.data))
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
  stopifnot(is.data.table(.data))
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
  stopifnot(is.data.table(.data))
  suffixes = vapply(.data[i, ], f, collapse = collapse, FUN.VALUE = character(1))
  setnames(.data, colnames(.data), paste(colnames(.data), na.omit(suffixes), sep = collapse))
  if (drop) {
    .data = .data[-i, ]
  }
  .data[]
}

# TODO: vectorize over varname and pattern
# #' Extract headers

#' Extract nested headers
#' 
#' @param .data A data.table, typically the result of `setDT(read_excel())`.
#' @param varname The name to assign the header.
#' @param pattern A regular expression matching header values. Case is ignored.
#' @export
#' @examples
#' d = data.table(c(NA, 'precinct', 'A', 'B'),
#'  c('Assembly', 'John Doe', '1', '2'),
#'  c(NA, 'Jane Doe', '3', '4'))
#'
#' extract_headers(d, 'office', 'assembly')
#' # The matching header is in row 1, column 2. Based on the missingness in
#' # row 1, column 3, the header applies both to columns 2 and 3.
extract_headers = function(.data, varname, pattern) {
    stopifnot(is.data.table(.data))
    stopifnot(is.character(varname) && length(varname) == 1)
    stopifnot(is.character(pattern) && length(pattern) == 1)

    # Search for `pattern` in each column of `.data`. The result is a list
    # giving the row (`i`), column (`j`) and `value` of the match for `pattern`.
    headers = purrr::imap(unname(.data), ~ {
      i = str_which(., stringr::regex(pattern, TRUE))
      ret = list(i, .y, .[i], varname)
      setNames(ret, c('i', 'j', 'value', 'varname'))
      }) %>%
      purrr::keep(~ length(.$i) > 0)

    # Headers apply to multiple columns; get the indexes of the columns between
    # a given match for `pattern` and the next match (or through the last
    # column) 
    headers = rbindlist(headers)
    j_ranges = interval_to_index(headers$j, length(.data), is_start = TRUE)
    if (nrow(headers) && length(j_ranges)) {
      # If `headers` were a zero-row table, this would be an error
      headers[, j_ranges := j_ranges]
    }
    headers[]
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
  full = setDT(readxl::read_excel(path, col_names = FALSE, sheet = sheet))
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
    tbl = setDT(readxl::read_excel(path, skip = .skip, n_max = .n_max, sheet = sheet,
        col_names = TRUE))
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
  stopifnot(is.data.table(.data))
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
  stopifnot(is.data.table(.data))
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
  stopifnot(is.data.table(.data))
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
#' drop_by_predicate(d, drop)
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
#' @export
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


#' Read an Excel file with multiple header rows
#' 
#' Deprecated; see new Excel vignette. Still used in ID.
#'
#' @import tidyxl
#' @importFrom purrr keep map
#' @inheritParams tidyxl::xlsx_cells
#' @param sheet Sheet to read. Either a character vector (the name of the sheet)
#' or an integer (the index of the sheet). If missing, defaults to the first
#' sheet.
#' @export
#' @examples
#' drop = list(
#'    function(d) { d$row >= 1130 },
#'    function(d) { d$character %=% 'total' })
#'
#' # Headers to extract via row number
#' top = list(office = 1:3, party = 4, candidate = 5)
#'
#' # Headers to extract via predicate
#' inner = list(
#'   jurisdiction = function(d) { d$col == 1 & d$row > 5 & d$local_format_id == 83 },
#'   precinct = function(d) { d$col == 1 & d$row > 5 & d$local_format_id != 83 })
#'
#' d = read_spreadsheet(path = '../data-ext/example-returns/16gen_stwd_pct.xlsx',"Federal",
#'   top, inner, drop)
#' head(d)
#'
#' max(d$row)
#' unique(d$office)
#' unique(d$party)
#' unique(d$candidate)
#'
#' head(unique(d$jurisdiction))
#' any(is.na(d$jurisdiction))
#'
#' any(is.na(d$precinct))
#' head(unique(d$precinct))
read_spreadsheet = function(path,
  top = list(candidate = 1),
  inner = list(),
  drop = list(),
  sheet,
  debug = TRUE) {

  if (missing(sheet)) {
    sheet = tidyxl::xlsx_sheet_names(path)[1]
  }
  d = setDT(tidyxl::xlsx_cells(path, sheets = sheet))
  d = combine_value_cols(d)

  # carry_down = function(.data, column = 'value') {
  #   d[, (h) := paste0(na.omit(value[top[[h]]]), collapse = ' '), by = 'col']
  #   d[trimws(get(h)) == '', (h) := NA]
  #   d[, (h) := zoo::na.locf(get(h), na.rm = FALSE)]
  # }

  for (h in names(top)) {
    d[, (h) := paste0(na.omit(value[top[[h]]]), collapse = ' '), by = 'col']
    d[trimws(get(h)) == '', (h) := NA]
    d[, (h) := zoo::na.locf(get(h), na.rm = FALSE)]
  }
  # Drop header rows
  d = d[!c(row) %in% unlist(top)]

  # Drop rows by predicate
  d = drop_by_predicate(d, drop)

  # d[, lag := value[row - 1] %=% 'Leg', by = 'col']
  # d[, lag_value := shift(value)]
  # jurisdiction = function(d) { d$col == 1 & d$row > 5 & d$local_format_id == 83 }
  # precinct = function(d) { d$col == 1 & d$row > 5 & d$local_format_id != 83 }

  drop_cells = lapply(names(inner), function(h) {
    # h = 'jurisdiction'
    id_cells = which(inner[[h]](d))
    d[id_cells, (h) := value]
    d[get(h) == '', (h) := NA]
    d[, (h) := zoo::na.locf(get(h), na.rm = FALSE)]
    id_cells
  })
  d = d[!unique(unlist((drop_cells)))]

  # Keep the header/identifier columns we've created, the value column, and
  # the position columns (row, col, address)
  d = d[, c(names(top), names(inner), 'value', 'row', 'col',
    'address'), with = FALSE]

  # Melt 
  d = melt(d, id = c(names(top), names(inner)),
    measure = list('value', 'row', 'col', 'address'),
    value.name = c('votes', 'row', 'col', 'address'), na.rm = TRUE)

  # With multiple measure columns, we don't need the variable column
  d[, variable := NULL]

  if (!isTRUE(debug)) {
    # Position columns not wanted
    d[, `:=`(row = NULL, col = NULL, address = NULL)]
  }
  # TODO: type.convert votes
  d[]
}

