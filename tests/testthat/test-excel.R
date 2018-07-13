for (name in c('common-toc', 'louisiana', 'merrimack-excerpt',
    'nebraska', 'new-hampshire', 'new-york-cortland', 'ohio', 'oregon',
    'south-dakota', 'utah-salt-lake', 'vermont', 'wisconsin')) {
  test_that('spreadsheet_example returns paths to packaged Excel spreadsheets',
    {
      expect_silent(spreadsheet_example(name))
    })
}

test_that('spreadsheet_example fails helpfully', {
  expect_error(spreadsheet_example('a'),
    '"a" is not the name of an example spreadsheet')
})

