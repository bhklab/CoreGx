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
#' @alias DataMapper-class
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
#' @alias LongTableDataMapper-class
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
    funContext <- .context(1)
    if (missing(rawdata)) stop(.errorMsg(funContext, 'The rawdata parameter ',
        'is mandatory. Please provide a list of raw data to map from.'))
    
    .LongTableDataMapper(rawdata=rawdata, rowDataMap=rowDataMap, 
        colDataMap=colDataMap, assayMap=assayMap, metadataMap=metadataMap)
}