#' Compare vote totals from precinct- and constituency-level returns
#'
#' @inheritParams write_precincts
#' @export
compare_aggregates = function(.data) {
  by_vars = c('state_postal', 'candidate')
  precinct_votes = .data[office == 'US President', .(precinct_votes = sum(votes)), by = by_vars]
  precinct_votes[, candidate := str_replace(candidate, 'la Riva', 'La Riva')]

  data(presidential_states_2016, package = 'elections', envir = environment())
  setDT(presidential_states_2016)
  state_votes = presidential_states_2016[state_postal == unique(precinct_votes$state_postal),
    .(state_votes = sum(votes)), by = by_vars]
  state_votes[, candidate := str_replace(candidate, '([^,]+), ([^,]+)', '\\2 \\1')]
  state_votes[, candidate := str_replace(candidate, ' [A-Z]\\. ', ' ')]
  state_votes[, candidate := str_replace(candidate, 'Estella ', '')]
  state_votes[, candidate := str_replace(candidate, 'Roque \"\"Rocky\"\"', 'Rocky')]

  comparison = merge(precinct_votes, state_votes, all = TRUE, by =
    c('state_postal', 'candidate'))
  comparison[, vote_diff := precinct_votes - state_votes]
  comparison[]
}
