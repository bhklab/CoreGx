c#' Subset a CoreSet object based on various parameters, such as cell lines, molecular features
#'
#' @param object An object inheriting from the `CoreGx::CoreSet` class
#' @param ... Allow definition of new arguments to this generic
#'
#' @export
setGeneric("subsetTo", function(object, ...) standardGeneric("subsetTo"))


##' Calculate the area under a dose-response curve
##'
##' @param object ?
##' @param ... Allow definition of new arguments to this generic
##'
##' @retrun [`numeric`] vector containing float value for the area under the dose-response curve
##'
##' @export
#setGeneric("computeAUC", function(object, ...) standardGeneric("computeAUC"))


#' Summarize across replicates for a sensitivity dose-response experiment
#'
#' @param object An object inheriting form the `CoreGx::CoreSet` class
#' @param ... Allow definition of new arguments to this generic
#'
#' @return [`data.frame`] containing treatment by cell line summary of a sensitivity experiment with values as the
#'     selected `sensitivity.measure`. Defaults `sensitivity.measure` is `auc_recomputed`.
#'
#' @export
setGeneric("summarizeSensitivityProfiles", function(object, ...) standardGeneric("summarizeSensitivityProfiles"))


#' Summarize molecular profile data such that there is a single entry for each cell line/treatment combination
#'
#' @param object An object inheriting form the `CoreGx::CoreSet` class
#' @param ... Allow definition of new arguments to this generic
#'
#' @return [`SummarizedExperiment`] containing the summarized molecular profile data
#'
#' @export
setGeneric("summarizeMolecularProfiles", function(object, ...) standardGeneric("summarizeMolecularProfiles"))


#' Get the annotations for a `Signature` class object, as returned by `drugSensitivitysig` or `radSensitivtySig` functions
#'    available in `PharmacoGx` and `RadioGx`, respectively.
#'
#' @param object A `Signature` class object
#' @param ... Allow definition of new arguments to this generic
#'
#' @return NULL Prints the signature annotations to console
#'
#' @export
setGeneric("showSigAnnot", function(object, ...) standardGeneric("showSigAnnot"))


#' Compute the correlation between a molecular feature and treatment response
#'
#' @param object An object inheriting form the `CoreGx::CoreSet` class
#' @param ... Allow definition of new arguments to this generic
#'
#' @return A 3D array of genes x drugs x metric
#'
#' @export
setGeneric("drugSensitivitySig", function(object, ...) standardGeneric("drugSensitivitySig"))

