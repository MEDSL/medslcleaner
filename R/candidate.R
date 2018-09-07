# Functions related to the `candidate` variable in precinct returns data

#' Normalize names of presidential candidates
#'
#' Limited to rows where `office == 'US President'`.
#' 
#' TODO: generalize, by parameterizing office and moving pattern-substitution
#' pairs to a data file.
#'
#' @inheritParams write_precincts
#' @export
normalize_presidential_candidates = function(.data) {
  if (!is.data.table(.data)) setDT(.data)
  .data[office == 'US President' & str_detect(candidate, coll('kennedy', TRUE)),
    candidate := 'Allyson Kennedy']
  .data[office == 'US President' & str_detect(candidate, coll('rocky', TRUE)),
    candidate := 'Rocky De La Fuente']
  .data[office == 'US President' & str_detect(candidate, coll('clinton', TRUE)),
    candidate := 'Hillary Clinton']
  .data[office == 'US President' & str_detect(candidate, coll('trump', TRUE)),
    candidate := 'Donald Trump']
  .data[office == 'US President' & str_detect(candidate, coll('mcmullin', TRUE)),
    candidate := 'Evan McMullin']
  .data[office == 'US President' & str_detect(candidate, coll('stein', TRUE)),
    candidate := 'Jill Stein']
  .data[office == 'US President' & str_detect(candidate, coll('castle', TRUE)),
    candidate := 'Darrell Castle']
  .data[office == 'US President' & str_detect(candidate, coll('johnson', TRUE)),
    candidate := 'Gary Johnson']
  .data[office == 'US President' & str_detect(candidate, coll('zoltan', TRUE)),
        candidate := 'Zoltan Gyurko']
  .data[office == 'US President' & str_detect(candidate, regex('la\\s*riva', TRUE)),
    candidate := 'Gloria La Riva']
  .data[, candidate := str_trim(str_replace_all(candidate, '\\s+', ' '))]
  .data[get('candidate') == '', candidate := NA]
  .data[]
}

#' Create a merge key from candidate names
#' 
#' Apply a normalizing transformation of the `candidate` column, creating
#' `candidate_normalized`. This is often a lower-cased last name.
#'
#' @inheritParams write_precincts
#' @export
candidate_merge_name = function(.data) {
  .data[, merge_name := trimws(tolower(candidate))]
  # Drop anything followed by a period (middle initials, suffixes)
  .data[, merge_name := gsub('(, )*[^ ]+\\.\\s*', '', merge_name)]
  # Flip last, first -> first, last
  .data[, merge_name := sub('([^,]+), ([^,]+)', '\\2 \\1', merge_name)]
  # Drop single (unpunctuated) initials
  .data[, merge_name := sub(' \\w ', ' ', merge_name)]
  .data[, merge_name := sub('\\s*\\(wr-in\\)', '', merge_name)]
  .data[, merge_name := sub('jr$', '', merge_name)]
  # Drop anything in parentheses
  .data[, merge_name := sub('\\s*\\([^\\(\\)]*\\)', '', merge_name)]
  # Drop anything in quotes
  .data[, merge_name := sub('\\s*"[^"]*"', '', merge_name)]
  # Drop any non-word characters at the end of the string
  .data[, merge_name := gsub('[^a-z]+$', '', merge_name)]
  .data[, merge_name := trimws(merge_name)]
  # Keep the last word (the one closest to the end of the string)
  .data[, merge_name := str_extract(tolower(merge_name), '\\w+$')]
  .data[]
}

#' Add candidate identifiers to precinct returns
#' 
#' Merges candidate IDs in the `[candidates]` dataset, by `state`, `district`,
#' `office`, and a normalizing transformation of `candidate`. (This remains in
#' the data afterward as `candidate_normalized`.)
#'
#' @inheritParams write_precincts
#' @import assertthat data.table stringr
#' @export
add_candidate_ids = function(.data) {

  # Idempotency
  if ('candidate_full' %in% names(.data)) {
    warning('Candidate identifiers already exist in the input CSV')
    # Drop existing candidate_ variables
    for (colname in str_subset(names(.data), 'candidate_')) {
      .data[, c(colname) := NULL]
    }
  }

  # Create a merge key from candidate names
  .data = candidate_merge_name(.data)
  setnames(.data, 'merge_name', 'candidate_normalized')
  data(candidates, envir = environment())

  # At least some state candidates should be in the lookup table
  assert_that(any(unique(.data$state_postal) %in% candidates$state_postal))

  # We also expect at least some observed districts to appear in the lookup table
  .data[, district := normalize_district_numbers(district)]
  assert_that(any(unique(.data$district) %in% candidates$district))

  # Only add the 'candidate' variables (merging on state, district, and office)
  keep = c(str_subset(names(candidates), 'candidate'), 'state_postal', 'district',
    'office')
  candidates = candidates[, c(keep), with = FALSE]

  by_vars = c('state_postal', 'candidate_normalized', 'district', 'office')
  stopifnot(!anyDuplicated(candidates, by = by_vars))
  merged = merge(.data, candidates, by = by_vars, all.x = TRUE)

  # We expect that at least some candidates were found in the lookup table
  stopifnot(nrow(merged[!is.na(candidate_full) | !is.na(candidate_fec)]) > 0)

  # Print proportion unmatched
  unique(merged[office %in% c('US House', 'US Senate', 'US President')], by =
    c('candidate', 'office', 'district'))[
    , .(unmatched = sum(!is.na(candidate_fec_name) | 
        !is.na(candidate_full)) / .N), by = 'office']

  merged[]
}

