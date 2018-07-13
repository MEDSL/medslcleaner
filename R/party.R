# Functions related to the `party` variable in precinct returns data

#' Expand common party abbreviations
#'
#' @inheritParams write_precincts
#' @export
normalize_party = function(.data) {
  assert_that(.data %has_name% 'party')
  .data = to_datatable(.data)
  .data[, party := stringr::str_replace_all(party,
    regex(c('^DEM$' = 'democratic',
      '^REP$' = 'republican',
      '^IND$' = 'independent',
      '^LIB$' = 'libertarian',
      '^LBT$' = 'libertarian',
      '^CON$' = 'constitution',
      '^GRE$' = 'green',
      '^GRN$' = 'green',
      '^ADP$' = 'american delta',
      "^NLP$" = "natural law party",
      "^NON$" = NA,
      "^NPA$" = NA,
      "^UST$" = "us taxpayers"), TRUE))]
  .data
}

#' @describeIn normalize_party The same operation, for a vector.
#' @param party A vector of party abbreviations.
#' @export
expand_party_abbr = function(party) {
  stringr::str_replace_all(party,
    regex(c('^DEM$' = 'democratic',
      '^REP$' = 'republican',
      '^IND$' = 'independent',
      '^LIB$' = 'libertarian',
      '^LBT$' = 'libertarian',
      '^CON$' = 'constitution',
      '^GRE$' = 'green',
      '^GRN$' = 'green',
      '^ADP$' = 'american delta',
      "^NLP$" = "natural law party",
      "^NON$" = NA,
      "^NPA$" = NA,
      "^UST$" = "us taxpayers"), TRUE))
}

