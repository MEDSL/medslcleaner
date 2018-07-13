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

test_that('validate_field tests for types', {
  # integer types
  expect_true(validate_field(data.frame(year = 2016), 'year'))
  expect_true(validate_field(data.frame(votes = 1:4), 'votes'))
  # boolean types
  expect_true(validate_field(data.frame(special = c(TRUE, FALSE)), 'special'))
  expect_error(validate_field(data.frame(special = c(0, 1)), 'special'),
    "should be logical")
  # constraints: 'required'
  expect_error(validate_field(data.frame(year = c(2016, NA)), 'year'))
  # type testing is forgiving across numerics
  expect_true(validate_field(data.frame(year = 2016.0), 'year'))
  # but no coercion is done; here strings are failure
  expect_error(validate_field(data.frame(year = '2016'), 'year'),
    "Values of year should be integer")
  # factors are not accepted in place of strings
  expect_error(validate_field(data.frame(stage = 'pri'), 'stage'),
    "Values of stage should be string")
})

test_that('validate_field tests for enumerated values', {
  expect_error(validate_field(data.frame(stage = 'primary', stringsAsFactors =
        FALSE), 'stage'), "Not all values of stage are enumerated possible values")
})

test_that('validate_field tests for existence in schema', {
  expect_error(validate_field(data.frame(a=1), 'votes'), 'does not have name')
})

test_that('validate_field tests for missingness ("required" attribute)', {
  expect_error(validate_field(data.frame(votes=c(1, NA)), 'votes'),
    'has missing values')
  expect_error(validate_field(data.frame(votes=c(1, NA)), 'votes'),
    'has missing values')
  expect_error(validate_field(data.frame(votes=c(NA_integer_)), 'votes'),
    'has missing values')
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

test_that('validate wraps validate_field for each field in fields', {
  data('wyoming', package = 'medslcleaner', envir = environment())
  validate(wyoming)
})

test_that('select_missing selects missing values', {
  expect_equal(nrow(select_missing(data.frame(votes = c(NA, '', ' ')),
        'votes')), 3)
  expect_message(nrow(select_missing(data.frame(votes = c(NA, '', ' ')),
        'votes')), '3/3 rows have missing "votes" values')
})

test_that('select_not_enumerated selects values not among those enumerated', {
  df = data.frame(stage = c(NA, 'primary'))
  expect_equal(select_not_enumerated(df, 'stage'), df)
  expect_message(select_not_enumerated(df, 'stage'), 
    '2/2 rows take "stage" values not enumerated in schema')
})

test_that('select_nonstring selects values not considered strings', {
  df = data.frame(candidate = c(NA, 'primary'))
  expect_message(select_nonstring(df, 'candidate'), 'is factor, not character')
  df = data.frame(candidate = 1:4)
  expect_message(select_nonstring(df, 'candidate'), 'is integer, not character')
  df = data.table(candidate = c(NA, 'primary'))
  expect_message(select_nonstring(df, 'candidate'), '0/2 rows')
})

test_that('select_noninteger selects values not considered integers', {
  df = data.table(votes = c(NA, 1))
  expect_message(select_noninteger(df, 'votes'), '0/2 rows')
  expect_equal(nrow(select_noninteger(df, 'votes')), 0)
  df = data.table(votes = c('A', 'B'))
  expect_message(select_noninteger(df, 'votes'), '2/2 rows')
  expect_equal(nrow(select_noninteger(df, 'votes')), 2)
})

test_that('field_schema retrieves field schema', {
  data('fields', package = 'medslcleaner', envir = environment())
  expect_equal(field_schema('votes'), fields[['votes']])
  expect_error(field_schema('foo'), 'not a schema-defined field')
})
