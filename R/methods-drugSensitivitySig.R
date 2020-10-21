#' Compute the correlation between a molecular feature and treatment response
#'
#' @param object An object inheriting form the `CoreGx::CoreSet` class
#' @param ... Allow definition of new arguments to this generic
#'
#' @return A 3D array of genes x drugs x metric
#'
#' @export
#' @keywords internal
setGeneric("drugSensitivitySig", function(object, ...) standardGeneric("drugSensitivitySig"))