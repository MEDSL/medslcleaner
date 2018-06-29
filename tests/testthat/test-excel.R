test_that('tidy() reshapes tables in tidy format', {
  data(warpbreaks, envir = environment())
  excerpt = warpbreaks[1:4, ]
  excerpt
  #   breaks wool tension
  # 1     26    A       L
  # 2     30    A       L
  # 3     54    A       L
  # 4     25    A       L
  # `row` gives table row index and `col` table column index 
  cells = tidy(copy(excerpt))
  expect_equal(names(cells), c('row', 'col', 'value'))
  expect_equal(nrow(cells), nrow(excerpt) * ncol(excerpt))
  expect_equal(cells$value[1:3], c('26', 'A', 'L'))
})

test_that('tidy() can consider colnames the first row in a table', {
  data(warpbreaks, envir = environment())
  excerpt = warpbreaks[1:4, ]
  # if use_names = TRUE, colnames represent the first row
  cells = tidy(copy(excerpt), use_names = TRUE)
  expect_equal(names(cells), c('row', 'col', 'value'))
  expect_equal(nrow(cells), nrow(excerpt) * ncol(excerpt) + length(excerpt))
  expect_equal(cells$value[1:3], c('breaks', 'wool', 'tension'))
})

test_that('as_header works as expected with down = TRUE', {
  tidy = data.table(
    value = letters[1:8],
    row = rep(1:4, each = 2),
    col = rep(1:2, times = 4))
  #    value row col
  # 1:     a   1   1
  # 2:     b   1   2
  # 3:     c   2   1
  # 4:     d   2   2
  # 5:     e   3   1
  # 6:     f   3   2
  # 7:     g   4   1
  # 8:     h   4   2
  tidy = as_header(tidy, 'id', rows = 1, down = TRUE, .drop = FALSE)
  expect_equal(tidy[col == 1, id], rep('a', 4))
  expect_equal(tidy[col == 2, id], rep('b', 4))
})

test_that('as_header works as expected with right = TRUE', {
  tidy = data.table(
    value = letters[1:8],
    row = rep(1:4, each = 2),
    col = rep(1:2, times = 4), key = c('row', 'col'))
  tidy[.(1, 2), value := NA] 
  #    value row col
  # 1:     a   1   1
  # 2:  <NA>   1   2
  # 3:     c   2   1
  # 4:     d   2   2
  # 5:     e   3   1
  # 6:     f   3   2
  # 7:     g   4   1
  # 8:     h   4   2
  tidy = as_header(tidy, 'id', rows = 1, right = TRUE, .drop = FALSE)
  expect_equal(tidy[row == 1, id], rep('a', 2))
})

test_that('as_header works as expected with right = TRUE and down = TRUE', {
  tidy = data.table(
    value = letters[1:8],
    row = rep(1:4, each = 2),
    col = rep(1:2, times = 4), key = c('row', 'col'))
  tidy[.(1, 2), value := NA] 
  tidy
  #    value row col
  # 1:     a   1   1
  # 2:  <NA>   1   2
  # 3:     c   2   1
  # 4:     d   2   2
  # 5:     e   3   1
  # 6:     f   3   2
  # 7:     g   4   1
  # 8:     h   4   2
  tidy = as_header(tidy, 'id', rows = 1, right = TRUE, down = TRUE, .drop =
    FALSE)
  expect_equal(tidy[['id']], rep('a', 8))
})

test_that('as_header works with cols', {
  tidy = data.table(
    row = rep(1:4, each = 2),
    col = rep(1:2, times = 4),
    key = c('row', 'col'))
  tidy[col == 1, value := letters[1:4]]
  tidy[col == 2, value := as.character(5:8)]
  tidy
  #    row col value
  # 1:   1   1     a
  # 2:   1   2     5
  # 3:   2   1     b
  # 4:   2   2     6
  # 5:   3   1     c
  # 6:   3   2     7
  # 7:   4   1     d
  # 8:   4   2     8
  tidy = as_header(tidy, 'precinct', cols = 1, right = TRUE, .drop = FALSE)
  expect_equal(tidy$precinct, rep(letters[1:4], each = 2))
})

test_that('na_row predicate works', {
  tidy = data.table(
    row = rep(1:4, each = 2),
    col = rep(1:2, times = 4))
  tidy[row == 1, value := NA_integer_]
  tidy[row > 1, value := 0L]
  tidy
  #    row col value
  # 1:   1   1    NA
  # 2:   1   2    NA
  # 3:   2   1     0
  # 4:   2   2     0
  # 5:   3   1     0
  # 6:   3   2     0
  # 7:   4   1     0
  # 8:   4   2     0
  expect_equal(all_na(tidy), c(rep(TRUE, 2), rep(FALSE, 6)))
})

test_that('na_row predicate works with cols', {
  tidy = data.table(
    row = rep(1:4, each = 2),
    col = rep(1:2, times = 4))
  tidy[row == 1, value := NA_integer_]
  tidy[row > 1, value := 0L]
  tidy[row == 2 & col == 2, value := NA_integer_]
  #    row col value
  # 1:   1   1    NA
  # 2:   1   2    NA
  # 3:   2   1     0
  # 4:   2   2    NA
  # 5:   3   1     0
  # 6:   3   2     0
  # 7:   4   1     0
  # 8:   4   2     0
  expect_equal(all_na(tidy, cols = 2), rep(c(TRUE, FALSE), each = 4))
})

test_that("spreadsheet_example fails gracefully", {
  expect_true(file.exists(spreadsheet_example('ohio')))
  expect_error(spreadsheet_example('ohiio'), '"ohiio" is not the name of an example spreadsheet')
})
