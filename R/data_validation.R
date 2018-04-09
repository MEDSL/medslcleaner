#' Expected Properties of Columns in MEDSL Precinct Returns
#' 
#' Defines a schema for column validity. The name of each list element is a
#' column that may occur in MEDSL precinct returns. Sub-elements describe
#' constraints to be imposed on these columns. (This validation is implemented
#' in `[write_precincts`]).
#' 
#' For example, the `dataverse` element of `precinct_validity` defines the
#' expected `type` of the column named `dataverse` in precinct returns:
#' `character`. It should not have `NA` values, so `no_na = TRUE`. In fact, it
#' should only take one of the enumerated `values`: `"president"`, `"senate"`,
#' etc.
#' 
#' @seealso `[write_precincts]`
'precinct_validity'

#' Expected column names in MEDSL precinct returns 
#' 
#' Generated from `[precinct_validity]`. 
#'
'precinct_columns'

#' Expected column types in MEDSL precinct returns
#' 
#' Generated from `[precinct_validity]`.
#'
'precinct_column_types'