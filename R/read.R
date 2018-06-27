# Functions for reading returns data from the disk

#' Read CSV-formatted returns from the disk
#'
#' Expected columns are read with the type given in
#' \code{\link{precinct_column_types}}. Parser warnings are promoted to errors
#' by default.
#'
#' @param path Path to a CSV.
#' @param strict Promote warnings to errors.
#' @param use_types Read expected columns with expected types. 
#' @export
read_precincts = function(path, strict = TRUE, use_types = FALSE) {
  if (isTRUE(use_types)) {
    # Subset type specification to the columns in the data
		headers = names(fread(path, nrows=0))
		col_types = purrr::keep(precinct_column_types, ~ .x %in% headers)
	} else {
		col_types = NULL
	}
  if (isTRUE(strict)) {
    # Promote warnings
		.data = withCallingHandlers({
			fread(path, colClasses = col_types)
		}, warning = function(w) stop(w))
	} else {
		.data = fread(path, colClasses = col_types)
	}
  .data = rename_legacy_vars(.data)
  assert_that(not_empty(.data))
  .data[]
}

#' Read a directory of returns data
#' 
#' A wrapper for the pattern:
#' 
#' - Locate paths with `list.files`, using `full.names = TRUE` and perhaps a `pattern`
#' - Apply a read function like [`fread`](data.table) or
#'   [`read_excel`](xlreader) to each path
#' - Combine the data with [`rbindlist`] or [`dplyr::bind_rows`], using the paths as an
#'   identifier column
#' 
#' @param path Path to a directory of returns.
#' @param ... Further arguments to \code{\link[base]{list.files}}.
#' @param f A function for reading files from disk.
#' @param idcol A name for the column in the result giving file paths.
#'
#' @export
read_dir = function(path = '../raw', f = data.table::fread, idcol = 'path', ...) {
  paths <- list.files(path, full.names = TRUE, ...)
  d = map(paths, f)
  d = map(d, setDT)
  d = setNames(d, paths)
  d = rbindlist(d, idcol = idcol)
  d
}

#' Rename legacy variable names
#'
#' @inheritParams read_precincts
#' @export
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

normalize_state = function(.data) {
  if (!is.data.table(.data)) setDT(.data)
  if (has_name(.data, 'state')) {
    data('state_ids', package = 'medslcleaner', envir = environment())
		if (all(.data[['state']] %chin% state_ids$state_postal)) {
			# We have a state variable whose values are state_postal codes
			setnames(.data, 'state', 'state_postal')
		}
	}
  .data[]
}
