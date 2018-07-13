diff_table = function(x, y) {
  x = to_values(x)
  y = to_values(y)
  xy = merge(x, y, by = c('i', 'variable'), all = FALSE, suffixes = c('',
      '_recode'))
  changes = xy[value != value_recode]
  changes[, i := NULL]
  changes = unique(changes, by = setdiff(names(changes), 'i'))
  setkeyv(changes, c('variable', 'value'))
  changes[]
}

to_values = function(.data) {
  .data = to_datatable(copy(.data))
  .data[, i := .I]
  data.table::melt(.data, id.vars = 'i', variable.factor = FALSE)
}

write_diff = function(.data, expr) {
  .data = to_datatable(copy(.data))
  result = eval(expr)
  diff_table(.data, result)[]
}
