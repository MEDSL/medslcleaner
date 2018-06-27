#! /usr/bin/env Rscript
#
# Add vote totals from Leip: https://uselectionatlas.org/RESULTS/ 

library(data.table)
library(rvest)

h = read_html('https://uselectionatlas.org/RESULTS/data.php?year=2016&datatype=national&def=1&f=1&off=0&elect=0')

d = html_node(h, '#datatable') %>%
  html_table(header = TRUE)

d = d[-c(1, 53, 54), c(3, 17:20)]
colnames(d) = c('state', 'Hillary Clinton', 'Donald Trump', 'Gary Johnson', 'Other')

setDT(d)
d = melt(d, id.vars = 'state', variable.name = 'candidate', variable.factor =
  FALSE, value.name = 'votes')

d[, votes := type.convert(gsub(',', '', votes))]
d[state == 'D. C.', state := 'District of Columbia']

data(state_ids, package = 'medslcleaner')
setDT(state_ids)
d = merge(d, state_ids[, .(state, state_postal)], by = 'state')
setcolorder(d, c('state', 'state_postal', 'candidate', 'votes'))

state_totals = d
devtools::use_data(state_totals, overwrite = TRUE)
