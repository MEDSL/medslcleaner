test_that('is_enumerated checks for values among those enumerated', {
  expect_true(is_enumerated(data.frame(state_postal = 'NY'), 'state_postal',
      c('AZ', 'NY')))
  expect_false(is_enumerated(data.frame(state_postal = 'AA'), 'state_postal',
      c('AZ', 'NY')))
  expect_error(assert_that(is_enumerated(data.frame(state_postal = 'AA'), 'state_postal',
      c('AZ', 'NY'))), 'Not all values of state_postal are enumerated possible values')
})

test_that('none_missing tests whether elements in a data variable are missing', {
  expect_true(none_missing(data.frame(a = c(1, 2)), 'a'))
  expect_false(none_missing(data.frame(a = c(1, NA)), 'a'))
  expect_error(assert_that(none_missing(data.frame(a = c(1, NA)),
          'a')), "a has missing values")
})

test_that('none_missing tests for missingness differently in strings', {
  expect_true(none_missing(data.frame(a = c(1, 2)), 'a'))
  expect_false(none_missing(data.frame(a = c(1, NA)), 'a'))
  expect_false(none_missing(data.frame(a = c('1', NA)), 'a'))
  expect_false(none_missing(data.frame(a = c('1', '')), 'a'))
  expect_error(assert_that(none_missing(data.frame(a = c('1', '')), 'a')),
    "a has missing values")
})

test_that('validate_field tests for types and constraints', {
  expect_true(validate_field(data.frame(year = 2016), 'year'))
  expect_error(validate_field(data.frame(year = c(2016, NA)), 'year'))
  expect_true(validate_field(data.frame(year = 2016L), 'year'))
  expect_error(validate_field(data.frame(year = '2016'), 'year'),
    "Values of year should be integer")
  expect_true(validate_field(data.frame(votes = 1:4), 'votes'))
  expect_error(validate_field(data.frame(stage = 'pri'), 'stage'),
    "Values of stage should be string")
  expect_error(validate_field(data.frame(stage = 'primary', stringsAsFactors =
        FALSE), 'stage'), "Not all values of stage are enumerated possible values")
})

test_that('select_nonstring selects the expected rows in .data', {
  expect_equal(nrow(select_nonstring(data.frame(a = c(1, 2)), 'a')), 2)
  expect_equal(nrow(select_nonstring(data.frame(a = c(1.2, NA)), 'a')), 2)
  expect_equal(nrow(select_nonstring(data.frame(a = 'b'), 'a')), 1)
})

test_that('select_noninteger selects the expected rows in .data', {
  expect_equal(nrow(select_noninteger(data.frame(a = c(1, 2)), 'a')), 0)
  expect_equal(nrow(select_noninteger(data.frame(a = c(1.2, 2)), 'a')), 1)
  expect_equal(nrow(select_noninteger(data.frame(a = c(1.2, NA)), 'a')), 1)
  expect_equal(nrow(select_noninteger(data.frame(a = 'b'), 'a')), 1)
})

test_that('select_not_enumerated selects expected values in .data', {
  selection = select_not_enumerated(data.table(stage = c('gen', 'general')), 'stage')
  expect_equal(selection[['stage']], 'general')
})

test_that('select_missing selects expected values in .data', {
  expect_equal(nrow(select_missing(data.table(candidate = c('', NA)),
        'candidate')), 2L)
  expect_equal(nrow(select_missing(data.table(candidate = c('f', NA)),
        'candidate')), 1L)
})

test_that('validate works as expected', {
  data('wyoming', package = 'medslcleaner', envir = environment())
  validate(wyoming)
})
