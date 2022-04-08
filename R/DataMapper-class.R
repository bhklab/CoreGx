#' @importClassesFrom S4Vectors Annotated list_OR_List
#' @importFrom data.table copy
NULL

#' An S4 Class For Mapping from Raw Experimental Data to a Specific S4 Object
#'
#' This object will be used as a way to abstract away data preprocessing.
#'
#' @section Slots:
#' * rawdata: A list-like object containing one or more pieces of raw data
#'   that will be processed and mapped to the slots of an `S4` object.
#' * metadata: A `List` of object level metadata.
#'
#' @md
#' @aliases DataMapper-class
.DataMapper <- setClass('DataMapper',
    contains=c('VIRTUAL', 'Annotated'),
    slots=list(rawdata='list_OR_List')
)

.local_class_2 <- 'DataMapper'

.docs_DataMapper_accessors <- function(...) .parseToRoxygen(
    "
    @title Accessing and modifying data in a `{class_}` object.

    @description
    Documentation for the various setters and getters which allow manipulation
    of data in the slots of a `{class_}` object.

    @param object A `{class_}` object to get or set data from.
    @param value A `list`-like object to assign to the rawdata slot. Should be
        a `data.frame` or `data.table` with the current implementation.

    @family DataMapper-accessors

    @return Accessors: See details
    @return Setters: An update `{class_}` object, returned invisibly.
    ",
    ...
)


# ==================================
# DataMapper Accessors Documentation
# ----------------------------------

#' @name DataMapper-accessors
#' @eval .docs_DataMapper_accessors(class_=.local_class_2)
NULL

#' @export
setGeneric('rawdata', function(object, ...) standardGeneric('rawdata'))

.docs_DataMapper_get_rawdata <- function(...) .parseToRoxygen(
    "
    @details
    __rawdata__: Get the raw data slot from a `{class_}` object. Returns
    a list-like containing one or more raw data inputs to the
    `{class_}` object.

    @md
    @aliases rawdata,{class_}-method
    @exportMethod rawdata
    ",
    ...
)

#' @rdname DataMapper-accessors
#' @aliases rawdata
#' @eval .docs_DataMapper_get_rawdata(class_=.local_class_2)
setMethod('rawdata', signature(object='DataMapper'), function(object) {
    copy(object@rawdata)
})

#' @export
setGeneric("rawdata<-", function(object, ..., value)
    standardgeneric("rawdata<-"))

.docs_DataMapper_set_rawdata <- function(...) .parseToRoxygen(
    "
    @details
    __rawdata__: Set the raw data slot from a `{class_}` object.
    __value__: The `list`-like object to set for the rawdata slot. Note: this
        currently only supports `data.frame` or `data.table` objects.

    @md
    @aliases rawdata<-,{class_},{class1_}-method
    @exportMethod rawdata<-
    ",
    ...
)

#' @rdname DataMapper-accessors
#' @aliases rawdata<-
#' @eval .docs_DataMapper_set_rawdata(class_=.local_class_2, class1_='ANY')
setReplaceMethod('rawdata', signature(object='DataMapper'),
        function(object, value) {
    object@rawdata <- value
    object
})
