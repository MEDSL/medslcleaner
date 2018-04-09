#!/usr/bin/env Rscript
# Create a lookup table for joining federal candidates in precinct returns data
# and their identifiers.

PRECINCT_DATA = '~/Dropbox (MIT)/2016-precinct-data/data'

suppressPackageStartupMessages(library(data.table))
library(feather)
library(readr)
library(stringr)
library(assertthat)
library(medslcleaner)

# Read member identifiers
members = read_feather('federal-legislators.feather')
members = unique(members, by = c('merge_name', 'state_postal', 'district'))
setDT(members)
members[type == 'rep', office := 'US House']
members[type == 'sen', office := 'US Senate']
devtools::use_data(members, overwrite = TRUE)

# Read FEC identifiers
fec = read_feather('fec.feather')
setDT(fec)
fec = fec[, .(candidate_fec, state_postal, district, merge_name,
  candidate_fec_name = candidate, office)]
fec[office == 'US Senate', district := 'statewide']
devtools::use_data(fec, overwrite = TRUE)

cand = rbindlist(list(members, fec), fill = TRUE)
candidates = unique(cand, by = c('state_postal', 'district', 'merge_name', 'office'))
setnames(candidates, 'merge_name', 'candidate_normalized')
candidates = normalize_whitespace(candidates)
devtools::use_data(candidates, overwrite = TRUE)

# # paths = list.files(PRECINCT_DATA, '2016-.*precinct.csv', recursive = TRUE, full.names =
#
# # Add candidate names as they appear in the FEC file by FEC ID. These already
# # exist for candidates not in the @unitedstates data, so we're adding them for
# # the candidates who are.
# merged = merge(merged, fec[, .(candidate_fec, candidate_fec_name)], by.x =
#   'candidate_fec.x', by.y = 'candidate_fec', all.x = TRUE, , all.y = FALSE)
#
# merged[is.na(candidate_fec_name.x), candidate_fec_name.x := candidate_fec_name.y]
# merged[is.na(candidate_fec.x), candidate_fec.x := candidate_fec.y]
# merged[, `:=`(candidate_fec_name.y = NULL, candidate_fec.y = NULL)]
# setnames(merged, c('candidate_fec_name.x', 'candidate_fec.x'), 
#   c('candidate_fec_name', 'candidate_fec'))
#
# # Confirm that nobody in the member data is missing from our data, if they ran
# # in 2016 (NB Bridenstine is an exception; he ran unopposed and doesn't appear
# # in our data)
# unmatched = merged[is.na(candidate) & merge_name != 'bridenstine' &
#   !is.na(candidate_full)]
# stopifnot(!nrow(unmatched))
#
# merged = unique(merged, by = c('merge_name', 'state_postal', 'district', 'office'))
# setnames(merged, 'merge_name', 'candidate_normalized')
#
# merged = normalize_whitespace(merged)
# write_feather(merged, 'candidates.feather')
