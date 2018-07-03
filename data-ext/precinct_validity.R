# Load directly from ../data, so this script can be run immediately after
# updating the candidates data, without reinstalling the package
load('../data/candidates.rda')
data('state_ids', package = 'elections')
data('county_ids', package = 'elections')

precinct_validity = list(
  year = list(
    type = 'integer',
    no_na = TRUE,
    n_unique = 1,
    values = seq.int(1976, 2018, 2)),
  state_postal = list(
    type = 'character',
    no_na = TRUE,
    n_unique = 1,
    values = state_ids$state_postal),
  stage = list(
    type = 'character',
    no_na = TRUE,
    n_unique = 1,
    values = c('gen', 'pri')),
  special = list(
    type = 'logical',
    no_na = TRUE),
  jurisdiction = list(
    type = 'character',
    no_na = TRUE,
    is_valid = function(s) {stringr::str_length(s) > 0}),
  precinct = list(
    type = 'character',
    no_na = FALSE),
  candidate = list(
    type = 'character',
    no_na = FALSE),
  office = list(
    type = 'character'),
  district = list(
    type = 'character'),
  writein = list(
    type = 'logical',
    no_na = TRUE),
  party = list(
    type = 'character'),
  mode = list(
    type = 'character',
    no_na = TRUE),
  votes = list(
    type = 'integer',
    no_na = TRUE,
    is_valid = function(x) {
      x >= 0 & x %% 1 == 0
    }),
  dataverse = list(
    type = 'character',
    no_na = TRUE,
    values = c('president', 'senate', 'house', 'state', 'local', 'all')))

devtools::use_data(precinct_validity, overwrite = TRUE)

precinct_columns = names(precinct_validity)
devtools::use_data(precinct_columns, overwrite = TRUE)
