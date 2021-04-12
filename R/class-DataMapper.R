#' @importClassesFrom S4Vectors List
setClassUnion("list_or_List", c('list', 'List'))

#' An S4 Class For Mapping from Raw Experimental Data to a Specific S4 Object
#' 
#' This object will be used as a way to abstract away data preprocessing.
#' 
#' @slot rawdata A list-like object containing one or more pieces of raw data
#'   that will be processed and mapped to the slots of an `S4` object.
#' 
#' @md
#' @aliases DataMapper-class
.DataMapper <- setClass('DataMapper', 
    contains=c('VIRTUAL', 'Annotated'), slots=list(rawdata='list_or_List'))

#' A generic function for extracting raw data from an `S4` object.
#' 
#' @param object An `S4` object to extract rawdata from.
#' 
#' @return The raw data object.
#' 
#' @md
#' @export
setGeneric('rawdata', function(object, ...) standardGeneric('rawdata'))
#'
#' Get the raw data slot from a `DataMapper` object.
#' 
#' @param object A `DataMapper` object to extract raw data from.
#' 
#' @return A list-like containing one or more raw data inputs to the 
#'   `DataMapper` object.
#' 
#' @md
#' @export
setMethod('rawdata', signature(object='DataMapper'), function(object) {
    object@rawdata
})

#' A Class for Mapping Between Raw Data and an `LongTable` Object
#' 
#' @inherit DataMapper-class
#' @slot rowDataMap A list of mappings from 
#' @slot colDataMap A list of mappings from
#' @slot assayMap A list of mappings from 
#' @slot metadataMap A list of mappings from
#' @slot metadata A list of mappings from
#' 
#' @aliases LongTableDataMapper-class
#' @keywords internal
#' @export
.LongTableDataMapper <- setClass('LongTableDataMapper', 
    contains=c('DataMapper'),
    slots=list(
        rowDataMap='list_or_List',
        colDataMap='list_or_List',
        assayMap='list_or_List',
        metadataMap='list_or_List'
    ))

#' Constructor for the `LongTableDataMapper` class, which maps from one or
#'   more raw experimental data files to the slots of a `LongTable` object.
#' 
#' @param rawdata A list-like
#' @param rowDataMap A list-like
#' @param colDataMap A list-like
#' @param assayMap A list-like
#' @param metadataMap A list-like
#' 
#' @md
#' @export
LongTableDataMapper <- function(rawdata, rowDataMap=list(), 
    colDataMap=list(), assayMap=list(), metadataMap=list())
{
    funContext <- '[CoreGx::LongTableDataMapper]\n\t'
    if (missing(rawdata)) stop(.errorMsg(funContext, 'The rawdata parameter ',
        'is mandatory. Please provide a list of raw data to map from.'))
    
    .LongTableDataMapper(rawdata=rawdata, rowDataMap=rowDataMap, 
        colDataMap=colDataMap, assayMap=assayMap, metadataMap=metadataMap)
}

# ---- LongTableDataMapper Accessors

# -- rowDataMap

#'
#' @param object An `S4` object to access the rowDataMap from.
#' @param ... Allow new parameters to be defined for this generic.
#'
#' @return 
#'
#' @md
#' @export
setGeneric('rowDataMap', function(object, ...) standardGeneric('rowDataMap'))
#'
#' @param object An `S4` object to access the rowDataMap from.
#' @param ... Allow new parameters to be defined for this generic.
#'
#' @return 
#'
#' @md
#' @export
setMethod('rowDataMap', signature(object='LongTableDataMapper'), function(object) 
{
    object@rowDataMap
})

#'
#' @param object An `S4` object to access the rowDataMap from.
#' @param ... Allow new parameters to be defined for this generic.
#' @param value
#'
#' @return 
#'
#' @md
#' @export
setGeneric('rowDataMap<-', function(object, ...) standardGeneric('rowDataMap'))
#'
#' @param object An `S4` object to access the rowDataMap from.
#' @param ... Allow new parameters to be defined for this generic.
#'
#' @return 
#'
#' @md
#' @export
setMethod('rowDataMap', signature(object='LongTableDataMapper'), function(object) 
{
    object@rowDataMap
})

# -- colDataMap

#'
#'
#' @param object An `S4` object to access the colDataMap from.
#' @param ... Allow new parameters to be defined for this generic.
#'
#' @return 
#'
#' @md
#' @export
setGeneric('colDataMap', function(object, ...) standardGeneric('colDataMap'))
#'
#'
#' @param object An `S4` object to access the rowDataMap from.
#' @param ... Allow new parameters to be defined for this generic.
#'
#' @return 
#' 
#' @md
#' @export
setMethod('colDataMap', signature(object='LongTableDataMapper'), function(object) 
{
    object@colDataMap
})

# -- assayMap

#'
#'
#' @param object An `LongTableDataMapper` object to access the assayMap from.
#' @param ... Allow new parameters to be defined for this generic.
#'
#' @return 
#'
#' @md
#' @export
setGeneric('assayMap', function(object, ...) standardGeneric('assayMap'))
#'
#'
#' @param object An `LongTableDataMapper` object to access the rowDataMap from.
#' @param ... Allow new parameters to be defined for this generic.
#'
#' @return 
#'
#' @md
#' @export
setMethod('assayMap', signature(object='LongTableDataMapper'), function(object) 
{
    object@assayMap
})

# -- metadataMap

#'
#' @param object An `S4` object to access the metadataMap from.
#' @param ... Allow new parameters to be defined for this generic.
#'
#' @return
#'
#' @md
#' @export
setGeneric('metadataMap', function(object, ...) standardGeneric('metadataMap'))
#'
#' @param object A `LongTableDataMapper` object to access the rowDataMap from.
#' @param ... Allow new parameters to be defined for this generic.
#'
#' @return
#'
#' @md
#' @export
setMethod('metadataMap', signature(object='LongTableDataMapper'), function(object) 
{
    object@metadataMap
})

