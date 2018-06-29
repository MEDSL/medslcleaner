#' `lapply` with named outputs
#' 
#' This is equivalent to `sapply(x, f, simplify = FALSE, USE.NAMES = TRUE)`.
#'
#' @inheritParams base::lapply
#' @param name_transform A function to transform `X` before using it as the
#'   output's names.
#' @export
nlapply = function(X, FUN, ..., name_transform = NULL) {
  ret = lapply(X = X, FUN = FUN, ...)
  if (is.function(name_transform)) {
    # Optionally apply a transformation to X before using X as output names
    list_names = name_transform(X)
  } else {
    # Otherwise use original X as output names
    list_names = X
  }
  setNames(ret, list_names)
}
