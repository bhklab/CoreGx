#' Summarize across replicates for a sensitivity dose-response experiment
#'
#' @examples
#' # Dummy examples to satisfy Bioc requiremnts
#' setMethod('summarizeSensitivityProfiles', signature('CoreSet'), function(object) object)
#'
#' @param object An object inheriting form the `CoreGx::CoreSet` class
#' @param ... Allow definition of new arguments to this generic
#'
#' @return [`data.frame`] containing treatment by cell line summary of a sensitivity experiment with values as the selected `sensitivity.measure`. Defaults `sensitivity.measure` is `auc_recomputed`.
#'
#' @export
#' @keywords internal
setGeneric("summarizeSensitivityProfiles", function(object, ...) standardGeneric("summarizeSensitivityProfiles"))


#' Summarize molecular profile data such that there is a single entry for each cell line/treatment combination
#'
#' @examples
#' # Dummy examples to satisfy Bioc requiremnts
#' setMethod('summarizeMolecularProfiles', signature('CoreSet'), function(object) object)
#'
#' @param object An object inheriting form the `CoreGx::CoreSet` class
#' @param ... Allow definition of new arguments to this generic
#'
#' @return [`SummarizedExperiment`] containing the summarized molecular profile data
#'
#' @export
#' @keywords internal
setGeneric("summarizeMolecularProfiles", function(object, ...) standardGeneric("summarizeMolecularProfiles"))


#' Get the annotations for a `Signature` class object, as returned by `drugSensitivitysig` or `radSensitivtySig` functions
#'    available in `PharmacoGx` and `RadioGx`, respectively.
#'
#' @examples
#' # Dummy examples to satisfy Bioc requirements
#' setMethod('showSigAnnot', signature("CoreSet"), function(object) object)
#'
#' @param object A `Signature` class object
#' @param ... Allow definition of new arguments to this generic
#'
#' @return NULL Prints the signature annotations to console
#'
#' @export
#' @keywords internal
setGeneric("showSigAnnot", function(object, ...) standardGeneric("showSigAnnot"))


