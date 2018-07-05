#' Compare vote totals
#'
#' Compare vote totals with a reference dataset. By default, the reference
#' totals are the [state_totals] dataset from [Dave Leip's Atlas of U.S.
#' Presidential Elections](https://uselectionatlas.org), for `candidate` values
#' of Donald Trump, Hillary Clinton, Gary Johnson, and "Total" (the sum of votes
#' for these three and all other candidates).
#'
#' We sum `votes` in each dataframe by the variables specified in argument `by`,
#' and then left-join `.data` and the reference dataframe on the same variables.
#'
#' @param .data Precinct returns dataframe.
#' @param reference A dataframe of reference vote totals. See details.
#' @param by A vector of shared column names in `.data` and `reference` to merge
#'   on.
#' @param all.y Perform a full join, instead of a left join. This will include
#'   in the result all rows from the reference dataset.
#' @return A copy of `.data` with state-candidate vote totals and their
#'   difference from the reference totals.
#' @export
#' @examples
#' # Use default reference totals from https://uselectionatlas.org
#' .data = data.frame(office = 'US President', candidate = c('Gary Johnson',
#'   'Donald Trump'), state_postal = 'AL', votes = c(44000, 1318200))
#' compare_votes(.data)
#'
#' # Use an arbitrary dataframe for reference totals
#' reference = data.frame(office = 'US President', candidate = c('Gary Johnson',
#'   'Donald Trump'), state_postal = 'AL', votes = c(43022, 1327200))
#' compare_votes(.data, reference)
compare_votes = function(.data, reference = NULL, by = c('state_postal',
    'office', 'candidate'), all.y = FALSE) {
  if (!is.data.table(.data)) setDT(.data)
  stopifnot(all(c(by, 'votes') %in% names(.data)))
  if (!is.null(reference)) {
    if (!is.data.table(reference)) setDT(reference)
    reference = copy(reference)
    stopifnot(all(c(by, 'votes') %in% names(reference)))
    reference = reference[office == 'US President', .(votes = sum(votes, na.rm =
        TRUE)), by = by]
    state_totals = reference
  } else {
    data('state_totals', envir = environment())
    setDT(state_totals)
    state_totals[, `:=`(office = 'US President', state = NULL)]
  } 
  .data = copy(.data)[, .(votes = sum(votes, na.rm = TRUE)), by = by] 
  .data = merge(.data, state_totals, all.x = TRUE, all.y = all.y, by = by,
    suffixes = c("", "_reference"))
  .data[, diff_reference := votes - votes_reference]
  .data[]
}

