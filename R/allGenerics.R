# ==== CoreSet

#' Summarize across replicates for a sensitivity dose-response experiment
#'
#' @examples
#' print("Generics shouldn't need examples?")
#'
#' @param object An `S4` object to summarize sensitivity profiles for.
#' @param ... Allow definition of new arguments to this generic
#'
#' @return Depends on the implemented method
#'
#' @export
setGeneric("summarizeSensitivityProfiles",
    function(object, ...) standardGeneric("summarizeSensitivityProfiles"))


#' Summarize molecular profile data such that there is a single entry for each
#'   sample line/treatment combination
#'
#' @examples
#' print("Generics shouldn't need examples?")
#'
#' @param object An `S4` object to summarize the molecular profiles for.
#' @param ... Allow definition of new arguments to this generic
#'
#' @return Depends on the implemented method
#'
#' @export
setGeneric("summarizeMolecularProfiles",
    function(object, ...) standardGeneric("summarizeMolecularProfiles"))


#' Get the annotations for a `Signature` class object, as returned by
#'   `drugSensitivitysig` or `radSensitivtySig` functions available in
#'   `PharmacoGx` and `RadioGx`, respectively.
#'
#' @examples
#' print("Generics shouldn't need examples?")
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
#' @examples
#' print("Generics shouldn't need examples?")
#'
#' @param object An `S4` object to get treatment response experiment
#'    annotations from.
#' @param ... Allow new arguments to be defined for this generic.
#'
#' @return Depends on the implemented method
#'
#' @export
setGeneric("sensitivityInfo",
    function(object, ...) standardGeneric("sensitivityInfo"))


#' sensitivityInfo<- Generic Method
#'
#' Generic function to get the annotations for a treatment response experiment
#'   from an S4 class.
#'
#' @examples
#' print("Generics shouldn't need examples?")
#'
#' @param object An `S4` object to set treatment response experiment
#'    annotations for.
#' @param ... Allow new arguments to be defined for this generic.
#' @param value The new treatment response experiment annotations.
#'
#' @return Depends on the implemented method
#'
#' @export
setGeneric("sensitivityInfo<-",
    function(object, ..., value) standardGeneric("sensitivityInfo<-"))


#' sensitivityRaw Generic Method
#'
#' Generic function to get the raw data array for a treatment response experiment
#'   from an S4 class.
#'
#' @examples
#' print("Generics shouldn't need examples?")
#'
#' @param object An `S4` object to extract the raw sensitivity experiment
#'     data from.
#' @param ... `pairlist`  Allow new parameters to be defined for this generic.
#'
#' @return Depends on the implemented method
#'
#' @export
setGeneric("sensitivityRaw",
    function(object, ...) standardGeneric("sensitivityRaw"))

#' sensitivityRaw<- Generic
#'
#' Generic function to set the raw data array for a treatment response experiment
#'   in an S4 class.
#'
#' @param object An `S4` object to extract the raw sensitivity data from.
#' @param ... `pairlist` Allow new parameters to be defined for this generic.
#' @param value An object containing dose and viability metrics to update
#'   the object with.
#'
#' @return Depends on the implemented method
#'
#' @export
setGeneric("sensitivityRaw<-",
    function(object, ..., value) standardGeneric("sensitivityRaw<-"))

#' sensitivityProfiles Generic
#'
#' A generic for sensitivityProfiles getter method
#'
#' @examples
#' print("Generics shouldn't need examples?")
#'
#' @param object The `S4` object to retrieve sensitivity profile summaries
#'   from.
#' @param ... `pairlist` Allow defining new arguments for this generic.
#'
#' @return Depends on the implemented method
#'
#' @export
setGeneric("sensitivityProfiles", function(object, ...) standardGeneric("sensitivityProfiles"))

#' sensitivityProfiles<- Generic
#'
#' A generic for the sensitivityProfiles replacement method
#'
#' @param object An `S4` object to update the sensitivity profile summaries
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
#' @param object An `S4` object to retrieve the names of sensitivty summary
#'    measurements for.
#' @param ... Fallthrough arguements for defining new methods
#'
#' @return Depends on the implemented method
#'
#' @export
setGeneric("sensitivityMeasures",
    function(object, ...) standardGeneric("sensitivityMeasures"))

#' sensitivityMeasures<- Generic
#'
#' Set the names of the sensitivity summary metrics available in an S4
#'   object.
#'
#' @examples
#' print("Generics shouldn't need examples?")
#'
#' @param object An `S4` object to update.
#' @param ... Allow new methods to be defined for this generic.
#' @param value A set of names for sensitivity measures to use to
#'   update the object with.
#'
#' @return Depends on the implemented method
#'
#' @export
setGeneric('sensitivityMeasures<-',
    function(object, ..., value) standardGeneric('sensitivityMeasures<-'))

#' sensitivitySlotToLongTable Generic
#'
#' Convert the sensitivity slot in an object inheriting from a CoreSet from a
#'   list to a LongTable.
#'
#' @examples
#' print("Generics shouldn't need examples?")
#'
#' @param object `CoreSet` Object inheriting from CoreSet.
#' @param ... Allow new arguments to be defined on this generic.
#'
#' @return A `LongTable` object containing the data in the sensitivity slot.
#'
#' @export
setGeneric('sensitivitySlotToLongTable',
    function(object, ...) standardGeneric('sensitivitySlotToLongTable'))

# ==== LongTable Class

#' Generic method for resetting indexing in an S4 object
#'
#' This method allows integer indexes used to maintain referential integrity
#'   internal to an S4 object to be reset. This is useful particularly after
#'   subsetting an object, as certain indexes may no longer be present in the
#'   object data. Reindexing removes gaps integer indexes and ensures that the
#'   smallest contiguous integer values are used in an objects indexes.
#'
#' @examples
#' print("Generics shouldn't need examples?")
#'
#' @param object `S4` An object to redo indexing for
#' @param ... `pairlist` Allow definition of new parameters to this generic.
#'
#' @return Depends on the implemented method
#'
#' @export
setGeneric('reindex', function(object, ...) standardGeneric('reindex'))

#' Build a LongTable object
#'
#' @examples
#' print("Generics shouldn't need examples?")
#'
#' @param from What to build the LongTable from?
#' @param ... `pairlist` Allow definition of new parameters for
#'     implementations of this generic.
#'
#' @return Depends on the implemented method
#'
#' @export
setGeneric('buildLongTable',
    function(from, ...) standardGeneric('buildLongTable'))


#' Perform aggregation over an S4 object, but return an object of the same
#' class.
#'
#' @examples
#' print("Generics shouldn't need examples?")
#'
#' @param x An `S4` object to endomorphically aggregate over.
#' @param ... `pairlist` Allow definition of new parameters for
#'     implementations of this generic.
#'
#' @return An object with the same class as `x`.
#'
#' @export
setGeneric("endoaggregate", function(x, ...) standardGeneric("endoaggregate"))

#' Retrieve a set of assayKeys
#'
#' @examples
#' print("Generics shouldn't need examples?")
#'
#' @param x An `S4` object.
#' @param ... `pairlist` Allow definition of new parameters for
#'     implementations of this generic.
#'
#' @return An object representing the "assayKeys" of an `S4` object.
#'
#' @export
setGeneric("assayKeys", function(x, ...) standardGeneric("assayKeys"))


#' Retrieve and assayIndex
#'
#' @examples
#' print("Generics shouldn't need examples?")
#'
#' @param x An `S4` object.
#' @param ... `pairlist` Allow definition of new parameters for
#'     implementations of this generic.
#'
#' @return An object representing the "assayIndex" of an `S4` object.
#'
#' @export
setGeneric("assayIndex", function(x, ...) standardGeneric("assayIndex"))

# ===== Other Generics


#' Retrieve the specified item from object internal metadata.
#'
#' Internal slot for storing metadata relevant to the internal operation of an
#'     S4 object.
#'
#' Warning: This method is intended for developer use and can be ignored by
#'   users.
#'
#' @examples
#' print("Generics shouldn't need examples?")
#'
#' @param object `S4` An object with an @.itern slot containing an environment.
#' @param x `character` One or more symbol names to retrieve from the
#'    object@.intern environment.
#' @param ... Allow new parmeters to be defined for this generic.
#'
#' @return Depends on the implemented method
#'
#' @export getIntern
setGeneric('getIntern',
    function(object, x, ...) standardGeneric('getIntern'))


#' Set the internal structural metadata for an S4 class
#'
#' @param object An R object to update internal structural metadata for.
#' @param value An `immutable_list` object, being a class union between `list`
#'   and `immutable` S3 classes.
#'
#' @examples
#' print("Generics shouldn't need examples?")
#'
#' @return Updates the object and returns invisibly.
#'
#' @keywords internal
setGeneric("getIntern<-",
    function(object, ..., value) standardGeneric("getIntern<-"))


#' Generic to access the row identifiers from
#'
#' @examples
#' print("Generics shouldn't need examples?")
#'
#' @param object `S4` An object to get row id columns from.
#' @param ... Allow new arguments to this generic.
#'
#' @return Depends on the implemented method.
#'
#' @export
setGeneric('rowIDs', function(object, ...) standardGeneric('rowIDs'))


#' Generic to access the row identifiers from
#'
#' @examples
#' print("Generics shouldn't need examples?")
#'
#' @param object `S4` An object to get row metadata columns from.
#' @param ... Allow new arguments to this generic.
#'
#' @return Depends on the implemented method.
#'
#' @export
setGeneric('rowMeta', function(object, ...) standardGeneric('rowMeta'))


#' Generic to access the row identifiers for an object.
#'
#' @examples
#' print("Generics shouldn't need examples?")
#'
#' @param object `S4` An object to get column id columns from.
#' @param ... ALlow new arguments to this generic
#'
#' @return Depends on the implemented method.
#'
#' @export
setGeneric('colIDs', function(object, ...) standardGeneric('colIDs'))


#' Generic to access the column identifiers for a rectangular object.
#'
#' @examples
#' print("Generics shouldn't need examples?")
#'
#' @param object `S4` An object to get column metadata columns from.
#' @param ... ALlow new arguments to this generic
#'
#' @return Depends on impemented method.
#'
#' @export
setGeneric('colMeta', function(object, ...) standardGeneric('colMeta'))


#' Generic to access the assay columns of a rectangular object.
#'
#' @examples
#' print("Generics shouldn't need examples?")
#'
#' @param object `S4` An object to get assay ids from.
#' @param ... Allow new arguments to this generic.
#'
#' @return Depends on the implemented method.
#'
#' @export
setGeneric('assayCols',
    function(object, ...) standardGeneric('assayCols'))


#' Generic to access the unique id columns in an S4 object used to
#'
#' @examples
#' print("Generics shouldn't need examples?")
#'
#' @param object An `S4` object to get id columns from.
#' @param ... Allow new arguments to this generic.
#'
#' @return Depends on the implemented method
#'
#' @export
setGeneric('idCols',
    function(object, ...) standardGeneric('idCols'))

##' Generic to access the build configuration for an S4 object.
##'
##' @param object `S4` The object to retireve the configuration from.
##'
##'
#setGeneric('getConfig', function(object, ...) standardGeneric(''))