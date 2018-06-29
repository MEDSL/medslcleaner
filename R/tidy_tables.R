# Functions for manipulating tables into tidy formats
# Left over from first attempt at spreadsheets; not exported

# Collapse rows of characters
#
# Concatenate rows column-wise into a single row.
#
# @param i A vector of row indexes.
# @param f Concatenation (or other) function.
# @param drop Drop all but the first row.
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

# Collapse cells
#
# Concatenate rows column-wise into a single row.
#
# @param .data A data.table.
# @param i A vector of row indexes.
# @param f Concatenation (or other) function.
# @param drop Drop all but the first row.
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

# Expand headers to include values in initial rows
# 
# @export
# @examples
# d = data.frame(A = c('B', 'C'), L = c('M', 'N'), stringsAsFactors = FALSE)
# d = expand_colnames(d, rows = 1:2)
# d
expand_colnames = function(.data, rows, f = paste_omit_na, collapse = ' ', drop = TRUE) {
  .data = to_datatable(.data)
  suffixes = vapply(.data[rows, ], f, collapse = collapse, FUN.VALUE = character(1))
  setnames(.data, colnames(.data), paste(colnames(.data), na.omit(suffixes), sep = collapse))
  if (drop) {
    .data = .data[-rows, ]
  }
  .data[]
}
