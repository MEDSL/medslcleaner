# Functions for reading returns data from the disk

#' Read spreadsheet data from Excel files
#'
#' @param path Path to the xlsx file.
#' @param keep_all Keep all variables returned by [tidyxl::xlsx_cells()],
#'   instead of only those typically needed. (See [keep_minimal()].)
#' @param ... Further arguments to [tidyxl::xlsx_cells()]
#' @export
#' @examples
#' path = spreadsheet_example('wisconsin')
#' d = read_xlreturns(path)
read_xlreturns = function(path, ..., keep_all = FALSE) {
  ret = do.call(tidyxl::xlsx_cells, c(path = path, list(...)))
  ret = combine_value_cols(setDT(ret))
  if (!isTRUE(keep_all)) {
    ret = keep_minimal(ret)
  }
  ret[]
}

#' Read CSV-formatted returns from the disk
#'
#' Parser warnings are promoted to errors and legacy variables are renamed.
#'
#' @param path Path to a CSV.
#' @param strict Promote warnings to errors.
#' @export
read_legacy_csv = function(path, strict = TRUE) {
  if (isTRUE(strict)) {
    # Promote warnings
		.data = withCallingHandlers({
			fread(path)
		}, warning = function(w) stop(w))
	} else {
		.data = fread(path)
	}
  .data = rename_legacy_vars(.data)
  assert_that(not_empty(.data))
  .data[]
}

#' Read a directory of returns data
#'
#' A wrapper for the pattern:
#'
#' - Locate paths with [base::list.files()], using `full.names = TRUE` and
#'   perhaps a `pattern`
#' - Apply a read function like [data.table::fread()] or [readxl::read_excel()]
#'   to each path
#' - Combine the data with e.g. [data.table::rbindlist()], using the paths as an
#'   identifier column
#'
#' @param path Path to a directory of returns.
#' @param ... Further arguments to `[list.files]`.
#' @param f A function for reading files from disk.
#' @param idcol A name for the column in the result giving file paths.
#'
#' @export
read_dir = function(path = '../raw', f = data.table::fread, idcol = 'path', ...) {
  paths <- list.files(path, full.names = TRUE, ...)
  d = map(paths, f)
  d = map(d, setDT)
  d = setNames(d, paths)
  d = rbindlist(d, idcol = idcol, fill = TRUE)
  d
}

