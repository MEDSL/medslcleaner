#' Detect pattern matches
#' 
#' \code{\%=\%} is made for subsetting dataframes. Matching is case-insensitive
#' and vectorized over \code{string}.
#'
#' This is almost identical to \code{\%like\%} in \code{data.table} but it
#' ignores case by default. We could use \code{grepl} or \code{str_detect}, but
#' we do a lot of subsetting, and \code{\%=\%} is as many as 30 characters
#' shorter when ignoring case, while maintaining readability.
#'
#' @param string A character vector.
#' @param pattern A regular expression.
#' @return A logical vector.
#' @export
#' @examples
#' c('apple', 'banana', 'pear') %=% 'pp'
#'
#' data(virginia_precincts)
#' virginia_precincts[OfficeTitle %=% 'house', .(OfficeTitle)]
`%=%` = function(string, pattern) {
  str_detect(string, stringr::regex(pattern, TRUE))
}

#' Remove pattern matches
#'
#' Remove a pattern match from a string. Think of this as regex-powered
#' string subtraction. Matching is case-insensitive and greedy, i.e., all
#' matches will be removed.
#'
#' The equivalent alternatives are `str_remove_all(x, regex(pattern, ignore_case
#' = TRUE))` (which this function wraps) or `gsub('pattern', '', x, ignore.case
#' = TRUE)`.
#'
#' @param string A character vector.
#' @param pattern A regular expression.
#' @return The vector `string` without any substrings matching `pattern`.
#' @export
#' @examples
#' 'APPLE' %-% 'l\\w' == 'APP'
#' 'BANANA' %-% 'a|n' == 'B'
`%-%` = function(string, pattern) {
  str_remove_all(string, regex(pattern, TRUE))
}

#' @describeIn grapes-grapes Alias for removing a pattern match
#' 
#' @inheritParams %-%
#' @export
`%r%` = `%-%`

#' Move a pattern match between columns
#' 
#' @inheritParams write_precincts
#' @inheritParams %=%
#' @param from Source column in `.data`.
#' @param to Destination column in `.data`.
#' @return The dataframe `.data` with new column `to`.
# '@export
move = function(.data, pattern, from, to) {
  .data = to_datatable(.data)
  n_affected = sum(str_detect(.data[[from]], regex(pattern, TRUE)))
  message(glue('{n_affected}/{nrow(.data)} matching rows in `{from}`'))
  .data[, (to) := str_extract(get(from), regex(pattern, TRUE))]
  .data[, (from) := str_remove(get(from), regex(pattern, TRUE))]
  n_target_na = sum(is.na(.data[[to]]))
  message(glue('{n_target_na}/{nrow(.data)} NA rows in `{to}`'))
  .data[]
}

#' Extract pattern matches
#'
#' This is an infix operator for case-insensitive substring extraction via
#' regex. Matching is non-greedy, i.e., only the first match is returned.
#'
#' It's implemented as  `str_extract(x, regex('pattern', TRUE))` and like
#' `sub(x, 'inverse-pattern', '', ignore.case = TRUE)`.
#'
#' @inheritParams %=%
#' @export
#' @examples
#' 'APPLE' %e% '\\w{3}' == 'APP'
#' 'BANANA' %e% 'a' == 'A'
`%e%` = function(string, pattern) {
  str_extract(string, regex(pattern, TRUE))
}

char_cols = function(.data) {
  names(.data)[sapply(.data, is.character)]
}

is_postal = function(x) {
  assert_that(is.character(x))
  state_ids = c(state.abb, 'DC')
  assert_that(all(x %chin% state_ids$state_postal))
}

