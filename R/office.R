# Functions related to the `office` variable in precinct returns data

#' Normalize office values
#' @export
normalize_office = function(.data) {
  setDT(.data)
  .data[office %=% 'president.*vice[\\s-]+president', office := 'US President']
  .data[office %=% 'u[\\s.]+s[\\s.]+ senat', office := 'US Senate']
  .data[office %=% 'united states senat', office := 'US Senate']
  .data[office %=% 'u[\\s.]+s[\\s.]+ (house|congress|represent)', office := 'US House']
  .data[office %=% 'governor', office := 'Governor']
  .data[]
}

