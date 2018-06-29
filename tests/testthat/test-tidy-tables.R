test_that('collapse_rows works', {
  skip('function dropped')
  d = data.frame(A = c('B', 'C'), L = c('M', 'N'), stringsAsFactors = FALSE)
  expect_error(collapse_rows(d, 1:2), 'is.data.table')
  setDT(d)
  ret = collapse_rows(d, 1:2)
  expect_equal(ret$A, 'B C')
  expect_equal(ret$L, 'M N')
})


test_that('expand_colnames works', {
  skip('function dropped')
  d = data.frame(A = c('B', 'C'), L = c('M', 'N'), stringsAsFactors = FALSE)
  expect_error(expand_colnames(d, 1:2), 'is.data.table')
  setDT(d)
  d = expand_colnames(d, 1:2)
  expect_equal(colnames(d), c('A B C', 'L M N'))
})

test_that('extract_headers extracts nested headers', {
  skip('function dropped')
  d = fread('../data-ext/example-returns/nested_headers.csv')
  ret = extract_headers(d, 'office', 'sheriff|attorney|treasurer')
  expect_is(ret, 'data.table')
  expect_true(all(ret$i == 2L))
  expect_equal(ret$j, c(2, 5, 7))
  expect_equal(ret$value, c('Sheriff', 'Attorney', 'Treasurer'))
  expect_equal(ret$varname, rep('office', 3))
  expect_equal(ret$j_ranges[[1]], 2:4)
})

