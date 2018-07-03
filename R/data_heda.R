#' A lookup table for Harvard Election Data Archive codes
#'
#' This dataset maps election variable codes in the 
#' [2014 HEDA documentation](https://dataverse.harvard.edu/file.xhtml?fileId=2456568&datasetVersionId=42865)
#' to values used in MEDSL returns.
#'
#' The naming convention for measure variables in HEDA tables is roughly
#' `eYYYY_OOO*_vv*`, where `e` gives election characteristics (e.g., general or
#' special); `YYYY` is a year; `OOO*` is a 3- or 4-letter office abbreviation;
#' and `vv*` is a suffix with other characteristics including party and vote
#' mode.
'heda_codes'
