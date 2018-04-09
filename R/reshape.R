#' Melt candidate votes
#'
#' A wrapper for `[melt]` in the common case where candidates are in columns and
#' cells give votes.
#' @export
melt_votes = function(data, ..., na.rm = TRUE, value.name = 'votes', variable.name = 'candidate',
  variable.factor = FALSE, value.factor = FALSE) {
  melt(data, ..., na.rm = na.rm, value.name = value.name, variable.name =
    variable.name, variable.factor = variable.factor, value.factor = FALSE)
}