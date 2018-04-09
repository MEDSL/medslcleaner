library(data.table)
library(feather)
library(medslcleaner)
library(readr)
library(stringr)

# Read FEC identifiers
fec = read_delim('fec.txt', delim = '|', col_names = c('candidate_fec', 'candidate',
    'CAND_ICI', 'PTY_CD', 'candidate_party', 'TTL_RECEIPTS',
    'TRANS_FROM_AUTH', 'TTL_DISB', 'TRANS_TO_AUTH', 'COH_BOP', 'COH_COP',
    'CAND_CONTRIB', 'CAND_LOANS', 'OTHER_LOANS', 'CAND_LOAN_REPAY',
    'OTHER_LOAN_REPAY', 'DEBTS_OWED_BY', 'TTL_INDIV_CONTRIB', 'state',
    'district', 'SPEC_ELECTION', 'PRIM_ELECTION', 'RUN_ELECTION',
    'GEN_ELECTION', 'GEN_ELECTION_PRECENT', 'OTHER_POL_CMTE_CONTRIB',
    'POL_PTY_CONTRIB', 'CVG_END_DT', 'INDIV_REFUNDS', 'CMTE_REFUNDS'))
setDT(fec)
fec = fec[, .(candidate_fec, candidate, candidate_party, state, district)]
fec = candidate_merge_name(fec)
fec = fec[, district := normalize_district_numbers(district)]
fec[str_sub(candidate_fec, 1, 1) == 'H', office := 'US House']
fec[str_sub(candidate_fec, 1, 1) == 'S', office := 'US Senate']
fec[str_sub(candidate_fec, 1, 1) == 'P', office := 'US President']
fec[office == 'US President', district := 'statewide']
# Duplicate the presidential candidates in each state; they otherwise have '00'
# for state, and we'll later merge on state
presidents_with_states = lapply(state.abb, function(state_abb) {
  copy(fec[office == 'US President'])[, state := state_abb]
})
fec = rbindlist(c(list(fec), presidents_with_states))
fec = unique(fec)
setnames(fec, 'state', 'state_postal')
write_feather(fec, 'fec.feather')
