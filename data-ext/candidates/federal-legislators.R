#!/usr/bin/env Rscript
# Extract and combine data on members of Congress downloaded 2018-03-31 from
# https://github.com/unitedstates/congress-legislators, for merging into MEDSL
# election returns.
library(data.table)
library(feather)
library(yaml)

# Pull out of the YAML the fields of interest
read_legislators = function(url) {
  members = read_yaml(url)
  members = lapply(members, function(x) {
    tryCatch({
      if (length(x$terms)) {
        last_term = x[['terms']][[length(x[['terms']])]]
        term_fields = c('party', 'state', 'type', 'start', 'end')
        if ('district' %in% names(last_term)) {
          term_fields = c(term_fields, 'district')
        }
        term_fields = last_term[term_fields]
      } else {
        term_fields = NULL
      }
      id_fields = x$id
      id_fields$fec = paste(id_fields$fec, collapse = '; ')
      data.frame(x$name, term_fields, id_fields, stringsAsFactors = FALSE)
    }, error = function(e) { warning(x); return(data.frame()) })
})
  members = rbindlist(members, fill = TRUE)
  members[, merge_name := tolower(last)][]
}

# Get current members who took office at the start of the session 
members = read_legislators('legislators-current.yaml')
members = members[start == '2017-01-03']

# Get historical members who either took office at the start of the session and
# have since vacated their seats, or who lost in 2016
historical = read_legislators('legislators-historical.yaml')
historical = historical[start == '2017-01-03' | end == '2017-01-03']

# Combine
members = rbindlist(list(current=members, former=historical), fill = TRUE,
  idcol='status')

# Check
stopifnot(!anyDuplicated(members, by = 'official_full'))

# Select and rename
members = members[,
  .(candidate_last = last,
    candidate_first = first,
    candidate_middle = middle,
    candidate_full = official_full,
    candidate_suffix = suffix,
    candidate_nickname = nickname,
    candidate_fec = fec,
    candidate_google = google_entity_id,
    candidate_govtrack = govtrack,
    candidate_icpsr = icpsr,
    candidate_maplight = maplight,
    candidate_opensecrets = opensecrets,
    candidate_wikidata = wikidata,
    candidate_party = party,
    state_postal = state,
    type, status, merge_name, district)
  ]
members[, district := as.character(district)]
members[type == 'sen', district := 'statewide']
stopifnot(!any(nrow(members[is.na(district)])))

write_feather(members, 'federal-legislators.feather')
