# Functions for working with geographic variables in precinct data

#' Add state and county identifiers to precinct returns
#'
#' @inheritParams write_precincts
#' @export
add_geo_ids = function(.data, must_work = TRUE) {
  initial_precinct_n = nrow(.data)
  stopifnot(!anyDuplicated(.data))
  .data = add_state_ids(.data)
  .data = standardize_county_fips(.data)
  .data = add_county_ids(.data, must_work = must_work)
  assert_that(nrow(.data) == initial_precinct_n)
  .data[]
}

# Add county identifiers to precinct returns
#
# @inheritParams write_precincts
# @export
add_county_ids = function(.data, must_work = TRUE) {
  if (!is.data.table(.data)) setDT(.data)
  data('county_ids', package = 'medslcleaner', envir = environment())
  stopifnot(!anyDuplicated(county_ids, by = c('county_fips', 'state_postal')))
  .data = merge(.data, county_ids, by = c('county_fips', 'state_postal'), all.x = TRUE)
  if (isTRUE(must_work)) {
    assert_that(noNA(.data$county_name))
    assert_that(all(nchar(.data$county_fips) == 5))
  }
  .data[]
}

# Standardize county FIPS codes
#
# @inheritParams write_precincts
# @export
standardize_county_fips = function(.data) {
  if (!is.data.table(.data)) setDT(.data)
  assert_that(has_name(.data, 'jurisdiction_fips'))
  if (!is.numeric(.data$jurisdiction_fips)) {
    .data[, jurisdiction_fips := type.convert(jurisdiction_fips)]
  }
  if (all(na.omit(.data$jurisdiction_fips) %% 1e5 == 0)) {
    # FIPS codes give counties
    .data[, county_fips := jurisdiction_fips /  1e5]
  } else {
    # FIPS codes give county subdivisions
    .data[, county_fips := floor(jurisdiction_fips / 1e5)]
  }
  .data[, county_fips := str_pad(as.character(county_fips), 5, 'left', '0')]
  .data[]
}

#' Merge FIPS codes from EAVS
#'
#' @param must_work Stop if any jurisdictions aren't matched to FIPS codes.
#' @inheritParams read_precincts
#' @export
add_jurisdiction_fips = function(.data, state_postal, add_key = TRUE, must_work = TRUE) {
  if (!is.data.table(.data)) setDT(.data)
  eavs = state_eavs(state_postal)
  if (isTRUE(add_key)) {
    .data = jurisdiction_merge_name(.data)
  }
  not_in_eavs(.data, eavs, must_work = must_work)
  not_in_state(.data, eavs, must_work = must_work)
  assert_that(!anyDuplicated(eavs, by = 'merge_name'))
  .data = merge(.data, eavs, by = 'merge_name', all.x = TRUE, all.y = FALSE)
  if (!noNA(.data$jurisdiction_fips)) {
    if (isTRUE(must_work)) {
      stop('Couldn\'t find in EAVS all jurisdictions in data')
    } else {
      warning('Couldn\'t find in EAVS all jurisdictions in data')
    }
  }
  .data[]
}

#' Load EAVS precinct data for a given state
#'
#' @inheritParams read_precincts
#' @export
state_eavs = function(state_postal) {
  .state = state_postal
  data(eavs, envir = environment())
  setDT(eavs)
  eavs = eavs[get('state_postal') == .state]
  eavs[, state_postal := NULL]
  eavs[]
}

#' Create a merge key for jurisdictions
#'
#' @inheritParams read_precincts
#' @export
jurisdiction_merge_name = function(.data) {
  if (!is.data.table(.data)) setDT(.data)
  .data[, merge_name := sub('\\s*county', '', jurisdiction, ignore.case = TRUE)]
  .data[, merge_name := sub('^W\\. ', 'West ', merge_name)]
  .data[, merge_name := sub('^S\\. ', 'South ', merge_name)]
  .data[, merge_name := sub('^N\\. ', 'North ', merge_name)]
  .data[, merge_name := sub('^E\\. ', 'East ', merge_name)]
  .data[, merge_name := trimws(tolower(merge_name))]
  .data[]
}

#' Display `jurisdiction` merge keys in EAVS but not precinct returns
#'
#' @param eavs A table of EAVS data.
#' @inheritParams read_precincts
#' @export
not_in_state = function(.data, eavs, must_work = TRUE) {
  if (!is.data.table(.data)) setDT(.data)
  stopifnot('merge_name' %in% names(.data))
  stopifnot('merge_name' %in% names(eavs))
  disjoint = sort(setdiff(unique(eavs$merge_name), unique(.data$merge_name)),
    na.last = FALSE)
  msg = glue('Not all state merge_names in EAVS merge_names; resolve in .data, ',
    'then pass `add_key = FALSE` to `add_jurisdiction_fips`. ',
    'Jurisdiction merge keys in EAVS and not returns: {paste(disjoint, collapse = ", ")}')
  if (length(disjoint) & must_work) {
    stop(msg)
  } else if (length(disjoint)) {
    warning(msg)
  }
  invisible(disjoint)
}

#' Display `jurisdiction` merge keys in precinct returns but not EAVS
#'
#' @inheritParams not_in_state
#' @export
not_in_eavs = function(.data, eavs, must_work = TRUE) {
  if (!is.data.table(.data)) setDT(.data)
  stopifnot('merge_name' %in% names(.data))
  stopifnot('merge_name' %in% names(eavs))
  disjoint = sort(setdiff(unique(.data$merge_name), unique(eavs$merge_name)),
    na.last = FALSE)
  msg = glue('Not all EAVS merge_names in state merge_names; resolve in .data, ',
    'then pass `add_key = FALSE` to `add_jurisdiction_fips`. ',
    'Jurisdiction merge keys in returns but not EAVS: {paste(disjoint, collapse = ", ")}')
  if (length(disjoint) & must_work) {
    stop(msg)
  } else if (length(disjoint)) {
    warning(msg)
  }
  invisible(disjoint)
}

# Add state identifiers to precinct returns
#
# Given the state postal code, add the full name of the state and its FIPS and
# ICPSR codes.
#
# @inheritParams write_precincts
add_state_ids = function(.data) {
  # We expect a single, known state abbreviation
  .data = normalize_state(.data)
  assert_that(has_name(.data, 'state_postal'))
  assert_that(is_postal(.data$state_postal))
  data('state_ids', package = 'medslcleaner', envir = environment())
  stopifnot(!anyDuplicated(state_ids, by = 'state_postal'))
  .data = merge(.data, state_ids, by = 'state_postal', all.x = TRUE)
  .data
}

# Add Census identifiers to precinct returns
#
# @inheritParams write_precincts
# @inheritParams add_jurisdiction_fips 
# @export
merge_census = function(.data, state_postal = NULL, must_work = TRUE) {
  if (!is.data.table(.data)) setDT(.data)
  data('census_precincts', package = 'medslcleaner', envir = environment())
  setDT(census_precincts)
  census = census_precincts[state == state_postal]
  if (!(all(unique(.data$merge_name) %in% unique(census$merge_name)))) {
    if (must_work) {
      stop("Not all merge names in .data in census")
    } else {
      warning("Not all merge names in .data in census")
    }
  }
  district_counties = unique(census, by = c('county_fips', 'merge_name', 'precinct'))
  n_districts = district_counties[, .(n = length(unique(county_fips))), by = c('merge_name')]
  multiple_counties = n_districts[n > 1]
  if (nrow(multiple_counties)) {
    print(multiple_counties)
    if (force) {
      warning("Districts span counties")
    } else {
      stop("Districts span counties")
    }
  }
  .data = jurisdiction_merge_name(.data)
  .data[, merge_precinct := gsub('[^0-9]', '', precinct)]
  .data = merge(.data, district_counties, by = c('merge_name', 'merge_precinct'), all.x = TRUE, all.y = FALSE)
  .data[, `:=`(merge_name = NULL, merge_precinct = NULL)]

  .data = fill_new_precincts(.data)
  new_precincts = unique(.data[is.na(vote_district_id), .(jurisdiction, precinct)])
  if (nrow(new_precincts)) {
    affected_jurisdictions = unique(new_precincts[, .(jurisdiction)])
    message(glue("Failed to match {i} precincts in {j} jurisdictions", i =
        nrow(new_precincts), j = nrow(affected_jurisdictions)))
  }
  .data[]
}

# Fill missing data for new precincts
#
# @inheritParams write_precincts
# @export
fill_new_precincts = function(.data) {
  if (!is.data.table(.data)) setDT(.data)
  .data[, county_fips := ifelse(is.na(county_fips), unique(na.omit(county_fips)),
    county_fips), by = c('jurisdiction')]
  .data[, state_fips := ifelse(is.na(state_fips), unique(na.omit(state_fips)),
    state_fips), by = c('jurisdiction')]
  .data[]
}
