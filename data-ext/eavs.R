library(haven)
library(data.table)

eavs = read_dta('eavs.dta')
setDT(eavs)
eavs = eavs[!type.convert(eavs$fipscode) %% 1e8 == 0]

# Modify the jurisdiction name for merging
eavs[, merge_name := sub('(town|county)$', '', juris, ignore.case = TRUE)]
eavs[, merge_name := tolower(trimws(merge_name))]
setnames(eavs,
  c('fipscode', 'juris', 'state'),
  c('jurisdiction_fips', 'jurisdiction_census', 'state_postal'))
eavs = unique(eavs)

devtools::use_data(eavs, overwrite = TRUE)
