#' @importClassesFrom S4Vectors List
setClassUnion("list_or_List", c('list', 'List'))

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
    contains=c('VIRTUAL', 'Annotated'), slots=list(rawdata='list_or_List'))

.local_class <- 'DataMapper'

.docs_DataMapper_accessors <- function(...) .parseToRoxygen(
    "
    @title Accessing and modifying data in a `{class_}` object.

    @description
    Documentation for the various setters and getters which allow manipulation
    of data in the slots of a `{class_}` object.

    @param object A `{class_}` object to get or set data from.
    @param ... See details.

    @family DataMapper-acessors

    @return Accessors: See details
    @return Setters: An update `{class_}` object, returned invisibly.
    ",
    ...
)

# ==================================
# DataMapper Accessors Documentation
# ----------------------------------

#' @name DataMapper-accessors
#' @eval .docs_DataMapper_accessors(class_=.local_class)
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
    @aliases rawdata,{class_}-method rawdata
    @exportMethod rawdata
    ",
    ...
)

#' @rdname DataMapper-accessors
#' @eval .docs_DataMapper_get_rawdata(class_=.local_class)
setMethod('rawdata', signature(object='DataMapper'), function(object) {
    object@rawdata
})

## FIXME:: Currently can't make DataMapper without rawdata, this doesn't
##   fit with the model where we preconfigure column maps to use with
##   new instances of rawdata from the same experimental design.