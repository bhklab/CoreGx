#' @include DataMapper-class.R

#' A Class for Mapping Between Raw Data and an `LongTable` Object
#'
#' @slot rowDataMap See Slots section.
#' @slot colDataMap See Slots section.
#' @slot assayMap See Slots section.
#' @slot metadataMap See Slots section.
#'
#' @inheritSection DataMapper-class Slots
#'
#' @section Slots:
#' * rowDataMap: A list of mappings from.
#' * colDataMap: A list of mappings from.
#' * assayMap: A list of mappings from .
#' * metadataMap: A list of mappings from.
#'
#' @md
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


## ===========================================
## LongTableDataMapper Accessors Documentation
## -------------------------------------------

.local_class <- 'LongTableDataMapper'

#' @name LongTableDataMapper-accessors
#' @eval .docs_DataMapper_accessors(class_=.local_class)
#' @eval .docs_DataMapper_get_rawdata(class_=.local_class) 
NULL

## =====================================
## LongTableDataMapper Accessors Methods
## -------------------------------------


## --------------------
## ---- rowDataMap slot


##
## -- rowDataMap

#' @rdname LongTableDataMapper-accessors
#'  
#'
#' @param object An `S4` object to access the rowDataMap from.
#' @param ... Allow new parameters to be defined for this generic.
#'
#' @return `list` of two `character` vectors, the first are the columns required
#'  to uniquely identify each row of a `LongTable` and the second any additional
#'  row-level metadata.
#'
#' @md
#' @export
setGeneric('rowDataMap', function(object, ...) standardGeneric('rowDataMap'))
#'
#' @title rowDataMap
#'
#' @param object An `S4` object to access the rowDataMap from.
#' @param ... Allow new parameters to be defined for this generic.
#'
#' @return `list` of two `character` vectors, the first are the columns required
#'  to uniquely identify each row of a `LongTable` and the second any additional
#'  row-level metadata.
#'
#' @md
#' @export
setMethod('rowDataMap', signature(object='LongTableDataMapper'), function(object) 
{
    object@rowDataMap
})

#' @title rowDataMap<-
#' 
#' @param object An `S4` object to access the colDataMap from.
#' @param ... Allow new parameters to be defined for this generic.
#' @param value A like-like object containing the values to assign to the
#'   `rowDataMap` slot of the `S4` object.
#'   
#' @return `invisible` Update object in-place and returns and invisible NULL.
#'
#' @md
#' @export
setGeneric('rowDataMap<-', function(object, ..., value) standardGeneric('rowDataMap<-'))
#'
#' @title rowDataMap<-
#' 
#' @param object A `LongTableDataMapper` object to access the rowDataMap from.
#' @param ... Allow new parameters to be defined for this generic.
#' @param value A `list` or `List` where the first item is the names of the
#'   identifier columns -- columns needed to uniquely identify each row in
#'   rowData -- and the second item is the metadata associated with those
#'   the identifier columns, but not required to uniquely identify rows in
#'   the object rowData.
#'
#' @return `invisible` Updates object and returns an invisible NULL.
#'
#' @md
#' @export
setReplaceMethod('rowDataMap', signature(object='LongTableDataMapper', 
    value='list_or_List'), function(object, value) 
{
    funContext <- '[CoreGx::`rowDataMap<-`,LongTableDataMapper-method]\n\t'
    rawdataCols <- colnames(rawdata(object))

    # -- Handle error conditions
    if (length(value) > 2) {
        stop(.errorMsg(funContext, 'Assignments to rowDataMap should be a list ',
            'of length 2, where the first item is the name of the id columns ',
            'and the second item is the name of the metadata columns which ',
            'map to those id columns.'))
    }

    hasIDcols <- value[[1]] %in% rawdataCols
    if (!all(hasIDcols)) {
        stop(.errorMsg(funContext, 'One or more of the id columns specified ',
            'in value[[1]] are not valid column names in the rawdata slot of ',
            'this ', class(object)[1], ' object!'))
    }
    
    if (length(value) > 1 && length(value[[2]]) != 0) {
        hasMetaCols <- value[[2]] %in% rawdataCols
        if (!all(hasMetaCols)) {
            stop(.errorMsg(funContext, 'The follow metadata columns in value[[2]] ',
                'are not present in rawdata(object): ', 
                .collapse(value[[2]][!hasMetaCols]), '!'))
        }
        hasOneToOneRelationship <- 
            value[[2]] %in% cardinality(rawdata(object), group=value[[1]])
        if (!all(hasOneToOneRelationship)) {
            stop(.errorMsg(funContext, 'The columns ', 
                .collapse(value[[2]][!hasOneToOneRelationship], ' do not have a ',
                '1:1 relationship with the specified ID columns!')))
        }
    }

    # -- Function body
    object@rowDataMap <- value
    return(object)
})

## --------------------
## ---- colDataMap slot

##
## -- colDataMap

#' @title colDataMap
#'
#' @param object An `S4` object to access the colDataMap from.
#' @param ... Allow new parameters to be defined for this generic.
#'
#' @return A `list` with two items, the first being all columns needed to
#'   uniquely identify each column in a `S4` object and the second any
#'   additional column-level metadata columns.
#'
#' @md
#' @export
setGeneric('colDataMap', function(object, ...) standardGeneric('colDataMap'))
#' @title colDataMap
#'
#' @param object An `S4` object to access the rowDataMap from.
#' @param ... Allow new parameters to be defined for this generic.
#'
#' @return A `list` with two items, the first being all columns needed to
#'   uniquely identify each column in a `LongTable` and the second any
#'   additional column-level metadata columns.
#' 
#' @md
#' @export
setMethod('colDataMap', signature(object='LongTableDataMapper'), function(object) 
{
    object@colDataMap
})

#' @title colDataMap<-
#'
#' @param object An `S4` object to access the colDataMap from.
#' @param ... Allow new parameters to be defined for this generic.
#' @param value A list-like object containing the values to assign to the
#'   `colDataMap` slot of the `S4` object.
#'
#' @return None, modifies the object.
#'
#' @md
#' @export
setGeneric('colDataMap<-', function(object, ..., value) standardGeneric('colDataMap<-'))
#' @title colDataMap<-
#'
#' @param object A `LongTableDataMapper` object to assign a colDataMap to.
#' @param value A `list` or `List` where the first item is the named of the
#'   identifier columns -- columns needed to uniquely identify each row in
#'   colData -- and the second item is the metadata associated with those
#'   the identifier columns, but not required to uniquely identify rows in
#'   the object colData.
#'
#' @return None, modifies the object.
#' 
#' @md
#' @export
setReplaceMethod('colDataMap', signature(object='LongTableDataMapper',
    value='list_or_List'), function(object, value) 
{
    funContext <- '[CoreGx::`colDataMap<-`,LongTableDataMapper-method]\n\t'
    rawdataCols <- colnames(rawdata(object))

    # -- Handle error conditions
    if (length(value) > 2 || !is.list(value)) {
        stop(.errorMsg(funContext, 'Assignments to colDataMap should be a list ',
            'of length 2, where the first item is the name of the id columns ',
            'and the second item is the name of the metadata columns which ',
            'map to those id columns.'))
    }

    hasIDcols <- value[[1]] %in% rawdataCols
    if (!all(hasIDcols)) {
        stop(.errorMsg(funContext, 'One or more of the id columns specified ',
            'in value[[1]] are not valid column names in the rawdata slot of ',
            'this ', class(object)[1], ' object!'))
    }
    
    if (length(value) > 1 && length(value[[2]]) != 0) {
        hasMetaCols <- value[[2]] %in% rawdataCols
        if (!all(hasMetaCols)) {
            stop(.errorMsg(funContext, 'The follow metadata columns in value[[2]] ',
                'are not present in rawdata(object): ', 
                .collapse(value[[2]][!hasMetaCols]), '!'))
        }
        hasOneToOneRelationship <- 
            value[[2]] %in% cardinality(rawdata(object), group=value[[1]])
        if (!all(hasOneToOneRelationship)) {
            stop(.errorMsg(funContext, 'The columns ', 
                .collapse(value[[2]][!hasOneToOneRelationship], ' do not have a ',
                '1:1 relationship with the specified ID columns!')))
        }
    }

    # -- Function body
    object@colDataMap <- value
    return(object)
})

## ----------------
## ---- assayMap slot

#' @title assayMap
#'
#' @param object An `LongTableDataMapper` object to access the assayMap from.
#' @param ... Allow new parameters to be defined for this generic.
#'
#' @return A `list` of character vectors. The name of each list item will be
#'   the assay in an `DataMapper` object that the columns in the `character` 
#'   vector will be assigned to.
#'
#' @md
#' @export
setGeneric('assayMap', function(object, ...) standardGeneric('assayMap'))
#' @title assayMap
#'
#' @param object An `LongTableDataMapper` object to access the rowDataMap from.
#' @param ... Allow new parameters to be defined for this generic.
#'
#' @return A `list` of character vectors. The name of each list item will be
#'   the assay in a `LongTableDataMapper` object that the columns in the 
#'   `character` vector will be assigned to.
#'
#' @md
#' @export
setMethod('assayMap', signature(object='LongTableDataMapper'), function(object) 
{
    object@assayMap
})

#' @title assayMap<-
#' 
#' @param object A `DataMapper`
#' @param ... Allow new arguments to be defined on this generic.
#' @param value A `list` of character vectors, where the name of each list
#'   item is the name of an assay and the values of each character vector
#'   specify the columns mapping to the assay in the `S4` object the 
#'   `DataMapper` constructs.
#'
#' @return `invisible` Updates object in-place and returns and invisible NULL.
#'
#' @md
#' @export
setGeneric('assayMap<-', function(object, ..., value) standardGeneric('assayMap<-'))
#' @title assayMap<-
#'
#' @param object A `LongTableDataMapper`
#' @param value A `list` of character vectors, where the name of each list
#'   item is the name of an assay and the values of each character vector
#'   specify the columns mapping to the assay in the `S4` object the 
#'   `LongTableDataMapper` constructs.
#'
#' @return `invisible` Updates object in-place and returns and invisible NULL.
#'
#' @md
#' @export
setReplaceMethod('assayMap', signature(object='LongTableDataMapper', 
    value='list_or_List'), function(object, value) 
{
    funContext <- '[CoreGx::`assayMap<-,LongTableDataMapper-method`]\n\t'
    rawdataCols <- colnames(rawdata(object))
    if (length(names(value)) == 0) stop(.errorMsg('The value argument must
        be a named list-like of column name character vectors!'))

    for (i in seq_along(value)) {
        hasRawdataCols <- value[[i]] %in% rawdataCols
        if (!all(hasRawdataCols)) {
            stop(.errorMsg(funContext, 'There are no columns named ',
                .collapse(value[[i]][!hasRawdataCols]), ' in the rawdata ',
                'of this ', class(object)[1], ' object. Please ensure item ',
                names(value)[i], ' of value has valid column names.'))
        }
    }

    object@assayMap <- value
    return(object)
})

# -- metadataMap

#' @title metadataMap
#' 
#' @param object An `S4` object to access the metadataMap from.
#' @param ... Allow new parameters to be defined for this generic.
#'
#' @return A `list` where each the name of each item 
#'
#' @md
#' @export
setGeneric('metadataMap', function(object, ...) standardGeneric('metadataMap'))
#' 
#' @title metadataMap
#'
#' @param object A `LongTableDataMapper` object to access the rowDataMap from.
#' @param ... Allow new parameters to be defined for this generic.
#'
#' @return A `list` of `character` vectors. Each item is an element of the
#'   constructed objects `@metadata` slot
#'
#' @md
#' @export
setMethod('metadataMap', signature(object='LongTableDataMapper'), function(object) 
{
    object@metadataMap
})

#' @title metadataMap<-
#'
#' @param object A `DataMapper` object.
#' @param ... Allow new parameters for this generic.
#' @param value A `list` of `character` vectors. The name of each list item
#'   is the name of the item in the `@metadata` slot of the `S4` object created
#'   when `metaConstruct` is called on the `DataMapper`, and the character
#'   vector specifies the columns of `@rawdata` to assign the item.
#'
#' @return `invisible` Updates object in-place, then returns and 
#'   `invisible(NULL)`.
#'
#' @md
#' @export
setGeneric('metadataMap<-', function(object, ..., value) standardGeneric('metadataMap<-'))
#'
#' @title metadataMap<-
#'
#' @param object A `DataMapper` object.
#' @param ... Allow new parameters for this generic.
#' @param value A `list` of `character` vectors. The name of each list item
#'   is the name of 
#'
#' @return `invisible` Updates object in-place, then returns and 
#'   `invisible(NULL)`.
#'
#' @md
#' @export
setReplaceMethod('metadataMap', signature(object='LongTableDataMapper', 
    value='list_or_List'), function(object, value) 
{
    funContext <- '[CoreGx::`metadataMap<-,LongTableDataMapper-method`]\n\t'
    rawdataCols <- colnames(rawdata(object))
    if (length(names(value)) == 0) stop(.errorMsg('The value argument must
        be a named list-like of column name character vectors!'))

    for (i in seq_along(value)) {
        hasRawdataCols <- value[[i]] %in% rawdataCols
        if (!all(hasRawdataCols)) {
            stop(.errorMsg(funContext, 'There are no columns named ',
                .collapse(value[[i]][!hasRawdataCols]), ' in the rawdata ',
                'of this ', class(object)[1], ' object. Please ensure item ',
                names(value)[i], ' of value has valid column names.'))
        }
    }

    object@metadataMap <- value
    return(object)
})