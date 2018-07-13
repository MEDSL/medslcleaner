test_that('diff_table returns differences between dataframes', {
  .x = data.frame(V1 = c(1, 1))
  .y = data.frame(V1 = c(1, 2))
  expected = data.table(variable = 'V1', value = 1, value_recode = 2,
    key = c('variable', 'value'))
  expect_equal(diff_table(.x, .y), expected)
})

test_that('diff_table returns unique differences', {
  .x = data.frame(V1 = c(1, 1, 1, 1))
  .y = data.frame(V1 = c(1, 1, 2, 2))
  expected = data.table(variable = 'V1', value = 1, value_recode = 2,
    key = c('variable', 'value'))
  expect_equal(diff_table(.x, .y), expected)
})

test_that('diff_table excludes unchanged columns', {
  .x = data.frame(V1 = c(1, 1, 1, 1), V2 = 1)
  .y = data.frame(V1 = c(1, 1, 2, 2), V2 = 1)
  expected = data.table(variable = 'V1', value = 1, value_recode = 2,
    key = c('variable', 'value'))
  expect_equal(diff_table(.x, .y), expected)
})


test_that('write_diff captures differences from expression', {
  .x = data.table(V1 = c(1, 1))
  diffs = write_diff(.x, {
    .x[1, V1 := 2][]
  })
  expected = data.table(variable = 'V1', value = 1, value_recode = 2,
    key = c('variable', 'value'))
  expect_equal(diffs, expected)
})

test_that('data.table updates by reference are made in the parent frame', {
  .x = data.table(V1 = c(1, 1))
  diffs = write_diff(.x, {
    .x[1, V1 := 2][]
  })
  expected = data.table(variable = 'V1', value = 1, value_recode = 2,
    key = c('variable', 'value'))
  expect_equal(.x, data.table(V1 = c(2, 1)))
})
