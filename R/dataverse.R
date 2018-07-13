# Functions related to the `dataverse` variable in precinct returns data

#' Assign dataverse identifiers by expected values of `office`.
#'
#' @inheritParams write_precincts
#' @export
assign_fixed = function(.data) {
  if (!is.data.table(.data)) setDT(.data)
  .data[office == 'US President', dataverse := 'president']
  .data[office == 'US Senate', dataverse := 'senate']
  .data[office == 'US House', dataverse := 'house']
  .data[office == 'State Senate', dataverse := 'state']
  .data[office == 'State House', dataverse := 'state']
  .data[office == 'Secretary of State', dataverse := 'state']
  .data[office == '(Lieutenant )*Governor', dataverse := 'state']
  .data[]
}

#' Assign rows matching an `office` pattern to a dataverse
#'
#' @inheritParams write_precincts
#' @inheritParams stringr::regex
#' @param dv A value to assign the `dataverse` variable for rows in `.data` that
#'   match `pattern` in column `office`.
#' @param ... Further arguments to `grepl`
#' @export
assign_match = function(.data, pattern, dv, ignore_case = TRUE, ...) {
  if (!is.data.table(.data)) setDT(.data)
  .data[grepl(pattern, office, ignore.case = ignore_case, ...), dataverse := dv]
  .data[]
}

#' Test an `office` pattern for assigning rows to a dataverse
#'
#' @inheritParams assign_match
#' @export
ogrep = function(.data, pattern, ignore_case = TRUE, ...) {
  if (!is.data.table(.data)) setDT(.data)
  matched = unique(.data[grepl(pattern, office, ignore.case = ignore_case, ...), .(office = substr(office, 1, 50), jurisdiction, dataverse)])
  matched = matched[, .(
    n_j = length(unique(jurisdiction)),
    dv = unique(dataverse)),
    by = 'office']
  matched[]
}

#' Test for `office` values not yet assigned to a dataverse
#' 
#' @return If successful, returns `NULL`; otherwise, prints unassigned values
#'   of `office` and stops.
#' @inheritParams write_precincts
#' @export
dvna = function(.data) {
  if (!is.data.table(.data)) setDT(.data)
  na_dv = unique(.data[is.na(dataverse), .(
    office = substr(office, 1, 50) # ,
    # n_j = length(unique(jurisdiction))
    )])
  print(na_dv)
  assert_that(nrow(na_dv) == 0)
}
