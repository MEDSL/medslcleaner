# Functions related to the `district` variable in precinct returns data

add_districts = function(.data) {
  # TODO
}

#' Drop leading zeroes from district numbers-as-strings
#' 
#' The `district` column is expected to be type `character`, but where its
#' values are entirely numeric, we would like these numbers to be formatted
#' consistently, especially for merging purposes (see `[add_candidate_ids]`).
#'
#' @export
normalize_district_numbers = function(x) {
  stopifnot(is.character(x))
  str_replace(x, '^0+([0-9]+)$', '\\1')
}

