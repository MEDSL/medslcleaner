expect_equal(read_double_header('V1,V2\nA,B'),
  data.table(candidate = c('V1', 'V2'), party = c('A', 'B')))

expect_equal(read_double_header('V1|V2\nA|B', sep = '|'),
  data.table(candidate = c('V1', 'V2'), party = c('A', 'B')))

expect_named(normalize_varnames(data.table(candidate.votes = 1)), 'votes')

# after_row = function(pattern, n = 1) {
#   function(d) {
#     d[, matches := character %=% pattern, by = 'col']
#     d[row %in% d[c(matches), row], match_row := TRUE]
#     d[, lag_row := shift(match_row, n, fill = FALSE, 'lag'), by = 'col']
#     d[, row %in% d[c(lag_row), row]]
#   }
# }
# leg_lag1 = after_row('st (sen|rep)', 1)
# # leg_lag1 = after_row('st (sen|rep)', 1)
# # leg_lag2 = after_row('st (sen|rep)', 2)
# # leg = read_spreadsheet('../raw/16gen_leg_pct.xlsx', 1, top = list(),
# #   inner = list(
# #     candidate_first = function(d) { d$col %in% 2:7 & leg_lag1(d)},
# #     candidate_last = function(d) { d$col %in% 2:7 & leg_lag2(d)},
# #     jurisdiction = function(d) { d$col == 1 & has_blank(d, 1) },
# #     precinct = function(d) { d$col == 1 & !has_blank(d, 0) },
# #     district = function(d) { d$col == 1 & str_detect(d$character, 'Leg') },
# #     office = function(d) { d$col %in% 2:7 & d$character %=% '^st (sen|rep)' }
# #     ),
# #   drop = list(function(d) {d$character %=% 'total'}))
# # leg
# # leg[row==2]
# # ]eg[row==1]
#
# leg[, party := str_extract(candidate_first, '^\\w')]
# leg[, candidate := paste(candidate_first, candidate_last) %-% '^\\w-']
# leg[, `:=`(candidate_first = NULL, candidate_last = NULL)]
# leg
#
# head(leg$precinct)
# su(leg$office)
# su(leg$district)
# su(leg$jurisdiction)
# su(leg$candidate)
# leg[row == 5]
# leg[row == 5]
#
