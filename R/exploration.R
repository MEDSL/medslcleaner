#' Syntactic sugar for clean-up of precinct returns 

#' Inspect variables' unique values
#'
#' @inheritParams write_precincts
#' @param n_max A limit on the number of unique values to return. `NULL` or `0`
#'   to disable.
#' @export
col_values = function(.data, n_max = 1000) {
  cols = setdiff(names(.data), c('votes', 'candidate.votes', 'total.votes'))
  cols = intersect(unique(c('precinct', 'jurisdiction', cols)), cols)
  lapply(.data[, cols, with = FALSE], function(x) {
    values = sort(unique(x), na.last = FALSE)
    if ((length(n_max) || n_max == 0) && length(values)) {
      values = values[1:min(length(values), n_max)]
    }
    values
  })
}

#' @describeIn col_values A shortcut for sort-unique
#' 
#' @param x A vector.
#' @export
su = function(x) {
  sort(unique(x), na.last = FALSE)
}

#' Search across character columns
#'
#' @inheritParams write_precincts
#' @inheritParams stringr::regex
#' @export
search_all = function (.data, pattern, ignore_case = TRUE) {
  result = lapply(char_cols(.data), function(colname) { 
    unique(str_subset(.data[[colname]], regex(pattern, ignore_case)))
  })
  result = setNames(result, char_cols(.data))
  result[sapply(result, length) > 0]
}
