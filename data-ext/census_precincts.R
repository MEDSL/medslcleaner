require(readr)
library(data.table)
library(stringr)

read_census = function(state_abbrev) {
  path = list.files(file.path('geo'), pattern = paste0('.*', toupper(state_abbrev), '_VTD'), full.names = TRUE)
  stopifnot(length(path) > 0)
  census = readr::read_delim(path, delim = '|')
  setDT(census)
  census[, merge_name := sub(' Precinct .*', '', NAME)]
  census[, merge_name := sub(' Ward .*', '', merge_name)]
  census[, merge_name := tolower(merge_name)]
  census[, merge_name := gsub('[^a-z ]', '', merge_name)]
  census[, merge_name := trimws(merge_name)]
  setnames(census, c('STATEFP', 'COUNTYFP', 'DISTRICT', 'NAME'),
    c('state_fips', 'county_fips', 'vote_district_id', 'vote_district'))
  census[, NAMELSAD := NULL]
  census[]
}

census_precincts = lapply(state.abb, read_census)
census_precincts = rbindlist(census_precincts)
census_precincts[, state_fips := type.convert(state_fips)]

census_precincts = merge(census_precincts, elections::state_ids, all.x = TRUE,
  by = 'state_fips')

setcolorder(census_precincts, (unique(c(
    str_subset(names(census_precincts), 'state'),
    str_subset(names(census_precincts), 'county'),
    str_subset(names(census_precincts), 'vote'),
    names(census_precincts)))))
census_precincts

devtools::use_data(census_precincts, overwrite = TRUE)
