# Functions related to the `office` variable in precinct returns data

#' Normalize office values
#'
#' @inheritParams write_precincts
#' @export
normalize_office = function(.data) {
  if (!is.data.table(.data)) setDT(.data)
  .data[office %=% 'president.*vice[\\s-]+president', `:=`(
    office = 'US President', district = 'statewide')]
  .data[office %=% 'u[\\s.]+s[\\s.]+ senat', `:=`(office = 'US Senate', district = 'statewide')]
  .data[office %=% 'united states senat', `:=`(office = 'US Senate', district = 'statewide')]
  .data[office %=% 'u[\\s.]+s[\\s.]+ (house|congress|represent)', office := 'US House']
  .data[office %=% 'governor', `:=`(office = 'Governor', district = 'statewide')]
  .data[office %=% 'str*aight party', office := 'Straight Party']
  .data[]
}

