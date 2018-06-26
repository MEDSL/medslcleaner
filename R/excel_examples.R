#' Retrieve paths to Excel file examples
#' @export
excel_example_path = function(name) {
  switch(name,
    'merrimack' = system.file('data-ext', 'example-returns', 
      'Merrimack-NH-2016_edits_small.xlsx', package = 'medslcleaner')
  )
}