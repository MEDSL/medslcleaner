test_that('collapse_rows works', {
  d = data.frame(A = c('B', 'C'), L = c('M', 'N'), stringsAsFactors = FALSE)
  expect_error(collapse_rows(d, 1:2), 'is.data.table')
  setDT(d)
  ret = collapse_rows(d, 1:2)
  expect_equal(ret$A, 'B C')
  expect_equal(ret$L, 'M N')
})

test_that('expand_colnames works', {
  d = data.frame(A = c('B', 'C'), L = c('M', 'N'), stringsAsFactors = FALSE)
  expect_error(expand_colnames(d, 1:2), 'is.data.table')
  setDT(d)
  d = expand_colnames(d, 1:2)
  expect_equal(colnames(d), c('A B C', 'L M N'))
})

test_that('extract_headers extracts nested headers', {
  d = fread('../data-ext/example-returns/nested_headers.csv')
  ret = extract_headers(d, 'office', 'sheriff|attorney|treasurer')
  expect_is(ret, 'data.table')
  expect_true(all(ret$i == 2L))
  expect_equal(ret$j, c(2, 5, 7))
  expect_equal(ret$value, c('Sheriff', 'Attorney', 'Treasurer'))
  expect_equal(ret$varname, rep('office', 3))
  expect_equal(ret$j_ranges[[1]], 2:4)
})

test_that('carving tidyxl works', {
  .data = tidyxl::xlsx_cells(system.file('data-ext/example-returns/16gen_stwd_pct.xlsx',
    package = 'medslcleaner', mustWork = TRUE), sheet = 'Federal')
  .data = setDT(.data) %>%
    combine_value_cols() %>%
    expand_headers(1:3) %>%
    keep_minimal()
  .data = .data %>%
    as_idcol('office', i = 1, down = TRUE, right = TRUE) %>%
    as_idcol('party', i = 4, down = TRUE) %>%
    as_idcol('precinct', j = 1, right = TRUE) %>%
    as_idcol('candidate', i = 5, down = TRUE) %>%
    as_idcol('jurisdiction', i = all_na(., by = 'row', j = seq(2, ncol(.))), j = 1, down =
      TRUE, right = TRUE) %>%
    filter(col != 1 & row > 6 & !all_na(., by = 'row', j = seq(2, ncol(.)))) %>%
    finalize()
  expect_is(.data$votes, 'integer')
  expect_equal(sum(.data$votes, na.rm = TRUE), 9355234)
})

test_that('as_idcol works as expected with down = TRUE', {
  tidy = data.table(
    value = letters[1:8],
    row = rep(1:4, each = 2),
    col = rep(1:2, times = 4))
  tidy = as_idcol(tidy, 'id', i = 1, down = TRUE)
  expect_equal(tidy[col == 1, id], rep('a', 4))
  expect_equal(tidy[col == 2, id], rep('b', 4))
})

test_that('as_idcol works as expected with right = TRUE', {
  tidy = data.table(
    value = letters[1:8],
    row = rep(1:4, each = 2),
    col = rep(1:2, times = 4), key = c('row', 'col'))
  tidy[.(1, 2), value := NA] 
  tidy = as_idcol(tidy, 'id', i = 1, right = TRUE)
  expect_equal(tidy[row == 1, id], rep('a', 2))
})

test_that('as_idcol works as expected with right = TRUE and down = TRUE', {
  tidy = data.table(
    value = letters[1:8],
    row = rep(1:4, each = 2),
    col = rep(1:2, times = 4), key = c('row', 'col'))
  tidy[.(1, 2), value := NA] 
  tidy = as_idcol(tidy, 'id', i = 1, right = TRUE, down = TRUE)
  expect_equal(tidy[['id']], rep('a', 8))
})

test_that('as_idcol works with j', {
  tidy = data.table(
    row = rep(1:4, each = 2),
    col = rep(1:2, times = 4),
    key = c('row', 'col'))
  tidy[col == 1, value := letters[1:4]]
  tidy[col == 2, value := as.character(5:8)]
  tidy = as_idcol(tidy, 'precinct', j = 1, right = TRUE)
  expect_equal(tidy$precinct, rep(letters[1:4], each = 2))
})

test_that('na_row predicate works', {
  tidy = data.table(
    row = rep(1:4, each = 2),
    col = rep(1:2, times = 4))
  tidy[row == 1, value := NA_integer_]
  tidy[row > 1, value := 0L]
  expect_equal(all_na(tidy), c(rep(TRUE, 2), rep(FALSE, 6)))
})

test_that('na_row predicate works with j', {
  tidy = data.table(
    row = rep(1:4, each = 2),
    col = rep(1:2, times = 4))
  tidy[row == 1, value := NA_integer_]
  tidy[row > 1, value := 0L]
  tidy[row == 2 & col == 2, value := NA_integer_]
  expect_equal(all_na(tidy, j = 2), rep(c(TRUE, FALSE), each = 4))
})
