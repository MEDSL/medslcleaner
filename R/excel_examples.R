#' Retrieve paths to packaged Excel spreadsheets
#'
#' The package includes spreadsheets for use in examples and vignettes.
#' Installation places them in the package directory. This function retrieves
#' the path to a spreadsheet, given one of the names below.
#'
#' Spreadsheet names: 
#' * `common-toc`
#' * `louisiana`
#' * `merrimack`
#' * `merrimack-excerpt`
#' * `nebraska`
#' * `new-hampshire`
#' * `new-york-cortland`
#' * `ohio`
#' * `oregon`
#' * `south-dakota`
#' * `utah-salt-lake`
#' * `vermont`
#' * `wisconsin`
#'
#' @param name Name of an Excel file example. See details.
#' @return Path to the example.
#' @export
#' @examples
#' # Paths will vary system-to-system
#' spreadsheet_example('merrimack-excerpt')
spreadsheet_example = function(name) {
  assert_that(is.string(name))
  switch(name,
    'merrimack' = system.file('merrimack-excerpt.xlsx', package =
      'medslcleaner'),
    'common-toc'  = system.file('common-toc.xlsx', package = 'medslcleaner'),
    'louisiana' = system.file('louisiana.xlsx', package = 'medslcleaner'),
    'merrimack-excerpt' = system.file('merrimack-excerpt.xlsx', package = 'medslcleaner'),
    'nebraska' = system.file('nebraska.xlsx', package = 'medslcleaner'),
    'new-hampshire' = system.file('new-hampshire.xlsx', package = 'medslcleaner'),
    'new-york-cortland' = system.file('new-york-cortland.xlsx', package = 'medslcleaner'),
    'ohio' = system.file('ohio.xlsx', package = 'medslcleaner'),
    'oregon' = system.file('oregon.xlsx', package = 'medslcleaner'),
    'south-dakota' = system.file('south-dakota.xlsx', package = 'medslcleaner'),
    'utah-salt-lake' = system.file('utah-salt-lake.xlsx', package = 'medslcleaner'),
    'vermont' = system.file('vermont.xlsx', package = 'medslcleaner'),
    'wisconsin' = system.file('wisconsin.xlsx', package = 'medslcleaner'),
    stop(deparse(name), ' is not the name of an example spreadsheet')
  )
}

