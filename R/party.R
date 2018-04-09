# Functions related to the `party` variable in precinct returns data

#' Expand common party abbreviations
#' 
#' TODO: generalize, by moving pattern-substitution pairs to a data file.
#'
#' @param party A vector of party abbreviations.
#' @export
expand_party_abbr = function(party) {
  # FIXME: should take and return .data for consistency
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
