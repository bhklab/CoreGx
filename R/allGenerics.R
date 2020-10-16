# ==== CoreSet

#' Summarize across replicates for a sensitivity dose-response experiment
#'
#' @param object An [`S4`] object to summarize sensitivity profiles for.
#' @param ... Allow definition of new arguments to this generic
#'
#' @export
setGeneric("summarizeSensitivityProfiles",
    function(object, ...) standardGeneric("summarizeSensitivityProfiles"))


#' Summarize molecular profile data such that there is a single entry for each
#'   cell line/treatment combination
#'
#' @param object An [`S4`] object to summarize the molecular profiles for.
#' @param ... Allow definition of new arguments to this generic
#'
#' @export
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
setGeneric("sensitivityInfo",
    function(object, ...) standardGeneric("sensitivityInfo"))


#' sensitivityInfo<- Generic Method
#'
#' Generic function to get the annotations for a treatment response experiment
#'   from an S4 class.
#'
#' @param object An [`S4`] object to set treatment response experiment
#'    annotations for.
#' @param ... Allow new arguments to be defined for this generic.
#' @param value The new treatment response experiment annotations.
#'
#' @export
setGeneric("sensitivityInfo<-",
    function(object, ..., value) standardGeneric("sensitivityInfo<-"))


#' sensitivityRaw Generic Method
#'
#' Generic function to get the raw data array for a treatment response experiment
#'   from an S4 class.
#'
#' @param object An [`S4`] object to extract the raw sensitivity experiment
#'     data from.
#' @param ... [`pairlist`]  Allow new parameters to be defined for this generic.
#'
#' @export
setGeneric("sensitivityRaw",
    function(object, ...) standardGeneric("sensitivityRaw"))

#' sensitivityRaw<- Generic
#'
#' Generic function to set the raw data array for a treatment response experiment
#'   in an S4 class.
#'
#' @param object An [`S4`] object to extract the raw sensitivity data from.
#' @param ... [`pairlist`] Allow new parameters to be defined for this generic.
#' @param value An object containing dose and viability metrics to update
#'   the object with.
#'
#' @export
setGeneric("sensitivityRaw<-",
    function(object, ..., value) standardGeneric("sensitivityRaw<-"))

#' sensitivityProfiles Generic
#'
#' A generic for sensitivityProfiles getter method
#'
#' @param object The [`S4`] object to retrieve sensitivity profile summaries
#'   from.
#' @param ... [`pairlist`] Allow defining new arguments for this generic.
#'
#'
#' @export
setGeneric("sensitivityProfiles", function(object, ...) standardGeneric("sensitivityProfiles"))

#' sensitivityProfiles<- Generic
#'
#' A generic for the sensitivityProfiles replacement method
#'
#' @param object An [`S4`] object to update the sensitivity profile summaries
#'    for.
#' @param ... Fallthrough arguments for defining new methods
#' @param value An object with the new sensitivity profiles. If a
#'   matrix object is passed in, converted to data.frame before assignment
#'
#' @return Updated \code{CoreSet}
#'
#' @export
setGeneric("sensitivityProfiles<-",
    function(object, ..., value) standardGeneric("sensitivityProfiles<-"))

#' sensitivityMeasures Generic
#'
#' Get the names of the sensitivity summary metrics available in an S4
#'   object.
#'
#' @examples
#' sensitivityMeasures(clevelandSmall_cSet)
#'
#' @param object An [`S4`] object to retrieve the names of sensitivty summary
#'    measurements for.
#' @param ... Fallthrough arguements for defining new methods
#'
#' @export
setGeneric("sensitivityMeasures",
    function(object, ...) standardGeneric("sensitivityMeasures"))

#' sensitivityMeasures<- Generic
#'
#' Set the names of the sensitivity summary metrics available in an S4
#'   object.
#'
#' @param object An [`S4`] object to update.
#' @param ... Allow new methods to be defined for this generic.
#' @param value A set of names for sensitivity measures to use to
#'   update the object with.
#'
#' @export
setGeneric('sensitivityMeasures<-',
    function(object, ..., value) standardGeneric('sensitivityMeasures<-'))

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
setGeneric('buildLongTable',
    function(from, ...) standardGeneric('buildLongTable'))


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
setGeneric('getIntern',
    function(object, x, ...) standardGeneric('getIntern'))

#' Generic to access the row identifiers from
#'
#' @param object [`S4`] An object to get row id columns from.
#' @param ... Allow new arguments to this generic.
#'
#' @export
setGeneric('rowIDs', function(object, ...) standardGeneric('rowIDs'))

#' Generic to access the row identifiers from
#'
#' @param object [`S4`] An object to get row metadata columns from.
#' @param ... Allow new arguments to this generic.
#'
#' @export
setGeneric('rowMeta', function(object, ...) standardGeneric('rowMeta'))

#' Generic to access the row identifiers for an object.
#'
#' @param object [`S4`] An object to get column id columns from.
#' @param ... ALlow new arguments to this generic
#'
#' @export
setGeneric('colIDs', function(object, ...) standardGeneric('colIDs'))

#' Generic to access the column identifiers for a rectangular object.
#'
#' @param object [`S4`] An object to get column metadata columns from.
#' @param ... ALlow new arguments to this generic
#'
#' @export
setGeneric('colMeta', function(object, ...) standardGeneric('colMeta'))

#' Generic to access the assay columns of a rectangular object.
#'
#' @param object [`S4`] An object to get assay ids from.
#' @param ... Allow new arguments to this generic.
#'
#' @export
setGeneric('assayCols',
    function(object, ...) standardGeneric('assayCols'))

##' Generic to access the build configuration for an S4 object.
##'
##' @param object [`S4`] The object to retireve the configuration from.
##'
##'
#setGeneric('getConfig', function(object, ...) standardGeneric(''))