# ==== CoreSet

#' Summarize across replicates for a sensitivity dose-response experiment
#'
#' @param object An object inheriting form the `CoreGx::CoreSet` class
#' @param ... Allow definition of new arguments to this generic
#'
#' @return [`data.frame`] containing treatment by cell line summary of a
#'   sensitivity experiment with values as the selected `sensitivity.measure`.
#'   Defaults `sensitivity.measure` is `auc_recomputed`.
#'
#' @export
#' @keywords internal
#' @noRd
setGeneric("summarizeSensitivityProfiles",
    function(object, ...) standardGeneric("summarizeSensitivityProfiles"))


#' Summarize molecular profile data such that there is a single entry for each cell line/treatment combination
#'
#' @param object An object inheriting form the `CoreGx::CoreSet` class
#' @param ... Allow definition of new arguments to this generic
#'
#' @return [`SummarizedExperiment`] containing the summarized molecular profile data
#'
#' @export
#' @keywords internal
#' @noRd
setGeneric("summarizeMolecularProfiles",
    function(object, ...) standardGeneric("summarizeMolecularProfiles"))


#' Get the annotations for a `Signature` class object, as returned by
#'   `drugSensitivitysig` or `radSensitivtySig` functions available in
#'   `PharmacoGx` and `RadioGx`, respectively.
#'
#' @param object A `Signature` class object
#' @param ... Allow definition of new arguments to this generic
#'
#' @return NULL Prints the signature annotations to console
#'
#' @export
#' @keywords internal
#' @noRd
setGeneric("showSigAnnot",
    function(object, ...) standardGeneric("showSigAnnot"))


#' Generic function to get the annotations for a treatment response experiment
#'   from an S4 class
#'
#' @param object An [`S4`] object to get treatment response experiment
#'    annotations from.
#' @param ... Allow new arguments to be defined for this generic.
#'
#' @export
#' @keywords internal
#' @noRd
setGeneric("sensitivityInfo",
    function(object, ...) standardGeneric("sensitivityInfo"))

#' Generic function to get the annotations for a treatment response experiment
#'   from an S4 class
#'
#' @param object An [`S4`] object to set treatment response experiment
#'    annotations for.
#' @param ... Allow new arguments to be defined for this generic.
#' @param value The new treatment response experiment annotations.
#'
#' @export
#' @keywords internal
#' @noRd
setGeneric("sensitivityInfo<-",
    function(object, ..., value) standardGeneric("sensitivityInfo<-"))

# ==== LongTable Class

#' Generic method for resetting indexing in an S4 object
#'
#' This method allows integer indexes used to maintain referential integrity
#'   internal to an S4 object to be reset. This is useful particularly after
#'   subsetting an object, as certain indexes may no longer be present in the
#'   object data. Reindexing removes gaps integer indexes and ensures that the
#'   smallest contiguous integer values are used in an objects indexes.
#'
#' @param object [`S4`] An object to redo indexing for
#' @param ... [`pairlist`] Allow definition of new parameters to this generic.
#'
#' @export
#' @keywords internal
#' @noRd
setGeneric('reindex', function(object, ...) standardGeneric('reindex'))

#' Build a LongTable object
#'
#' @param from What to build the LongTable from?
#' @param ... [`pairlist`] Allow definition of new parameters for
#'     implementations of this generic.
#'
#' @export
#' @keywords internal
#' @noRd
setGeneric('buildLongTable', function(from, ...) standardGeneric('buildLongTable'))


# ===== Other Generics

#' Retrieve the symbol for the object@.intern slot
#'
#' Internal slot for storing metadata relevant to the internal operation of an
#'     S4 object.
#'
#' Warning: This method is intended for developer use and can be ignored by
#'   users.
#'
#' @param object [`S4`] An object with an @.itern slot containing an environment.
#' @param x [`character`] One or more symbol names to retrieve from the
#'    object@.intern environment.
#'
#' @export
#' @keywords internal
#' @noRd
setGeneric('getIntern', function(object, x, ...) standardGeneric('getIntern'))

#' Generic to access the row identifiers from
#'
#' @param object [`S4`] An object to get row id columns from.
#' @param ... Allow new arguments to this generic.
#'
#' @export
#' @noRd
setGeneric('rowIDs', function(object, ...) standardGeneric('rowIDs'))

#' Generic to access the row identifiers from
#'
#' @param object [`S4`] An object to get row metadata columns from.
#' @param ... Allow new arguments to this generic.
#'
#' @export
#' @keywords internal
#' @noRd
setGeneric('rowMeta', function(object, ...) standardGeneric('rowMeta'))

#' Generic to access the row identifiers for an object.
#'
#' @param object [`S4`] An object to get column id columns from.
#' @param ... ALlow new arguments to this generic
#'
#' @export
#' @noRd
setGeneric('colIDs', function(object, ...) standardGeneric('colIDs'))

#' Generic to access the column identifiers for an object.
#'
#' @param object [`S4`] An object to get column metadata columns from.
#' @param ... ALlow new arguments to this generic
#'
#' @export
#' @noRd
setGeneric('colMeta', function(object, ...) standardGeneric('colMeta'))

#' Generic to access the assay columns of an object.
#'
#' @param object [`S4`] An object to get assay ids from.
#' @param ... Allow new arguments to this generic.
#'
#' @export
#' @noRd
setGeneric('assayCols', function(object, ...) standardGeneric('assayCols'))

##' Generic to access the build configuration for an S4 object.
##'
##' @param object [`S4`] The object to retireve the configuration from.
##'
##'
#setGeneric('getConfig', function(object, ...) standardGeneric(''))