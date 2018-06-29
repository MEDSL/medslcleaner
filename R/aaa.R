# This file sourced before all others

# Make these functions, used frequently in the package source, available in the
# package namespace
`%chin%` <- data.table::`%chin%`
`:=` <- data.table::`:=`
are_equal <- assertthat::are_equal
assert <- assertthat::assert_that
assert_that <- assertthat::assert_that
copy <- data.table::copy
data.table <- data.table::data.table
fread <- data.table::fread
glue <- glue::glue
has_name <- assertthat::has_name
`%has_name%` <- assertthat::`%has_name%`
is.count <- assertthat::is.count
is.data.table <- data.table::is.data.table
is.flag <- assertthat::is.flag
is.readable <- assertthat::is.readable
is.scalar <- assertthat::is.scalar
is.string <- assertthat::is.string
keep <- purrr::map
map <- purrr::map
noNA <- assertthat::noNA
not_empty <- assertthat::not_empty
regex  <- stringr::regex
rbindlist <- data.table::rbindlist
setDT <- data.table::setDT
setkeyv <- data.table::setkeyv
setnames <- data.table::setnames
str_detect <- stringr::str_detect
str_extract <- stringr::str_extract
str_length <- stringr::str_length
str_replace_all <- stringr::str_replace_all
str_remove <- stringr::str_remove
str_remove_all <- stringr::str_remove_all
str_subset <- stringr::str_subset
validate_that <- assertthat::validate_that
imap <- purrr::imap
imap_int <- purrr::imap_int
map <- purrr::map
lmap <- purrr::lmap
keep <- purrr::keep

# Don't let R CMD check complain about these variables when referenced unquoted
utils::globalVariables(c('.', 'candidate', 'candidate_fec', 'candidate_full',
    'census_precincts', 'county_fips', 'data', 'dataverse', 'district',
    'jurisdiction', 'jurisdiction_fips', 'merge_name', 'merge_precinct', 'n',
    'n_unique', 'na.omit', 'office', 'packageVersion', 'precinct',
    'precinct_path', 'presidential_states_2016', 'setNames', 'special', 'stage',
    'state', 'state_fips', 'state_postal', 'total.votes', 'type.convert',
    'vote_diff', 'vote_district_id', 'votes', 'writein', ".data", "V1",
    "candidate_fec_name", "county_ids", "data_type", "diff_reference",
    "group_by", "i", "precinct_column_types", "precinct_validity", "state.abb",
    "state_ids", "value", "votes_reference", "party", "fields"))

