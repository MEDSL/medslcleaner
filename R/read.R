# Functions for reading returns data from the disk

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
#' - Locate paths with `list.files`, using `full.names = TRUE` and perhaps a `pattern`
#' - Apply a read function like [`fread`](data.table) or [`read_excel`](xlreader) to each path
#' - Combine the data with e.g. [`rbindlist`], using the paths as an identifier column
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
  d = rbindlist(d, idcol = idcol, fill = TRUE)
  d
}

#' Rename legacy variable names
#'
#' FIXME: This function also drops variables, so its name is misleading.
#'
#' @inheritParams write_precincts
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

# We sometimes encounter in old CSVs a state variable whose values are actually
# state_postal codes
normalize_state = function(.data) {
  if (!is.data.table(.data)) setDT(.data)
  if (has_name(.data, 'state')) {
    data('state_ids', package = 'medslcleaner', envir = environment())
		if (all(.data[['state']] %chin% state_ids$state_postal)) {
			setnames(.data, 'state', 'state_postal')
		}
	}
  .data[]
}
