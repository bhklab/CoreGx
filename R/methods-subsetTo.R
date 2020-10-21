#' Subset a CoreSet object based on various parameters, such as cell lines, molecular features
#'
#' @param object An object inheriting from the `CoreGx::CoreSet` class
#' @param ... Allow definition of new arguments to this generic
#'
#' @return A subsetted version of the original `object`
#'
#' @export
#' @keywords internal
setGeneric("subsetTo", function(object, ...) standardGeneric("subsetTo"))

