#' @include CoreSet-class.R allGenerics.R
NULL

.local_class <- 'CoreSet'
.local_data <- 'clevelandSmall_cSet'
data('clevelandSmall_cSet')

# =======================================
# Accessor Method Documentation Object
# ---------------------------------------

#' @noRd
.docs_CoreSet_accessors <- function(...) .parseToRoxygen(
    "
    @title Accessing and modifying information in a `{class_}`

    @description
    Documentation for the various setters and getters which allow manipulation
    of data in the slots of a `{class_}` object.

    @param object A `{class_}` object.
    @param value See details.
    @param mDataType `character(1)` The name of a molecular datatype to access
    from the `molecularProfiles` of a `{class_}` object.
    @param assay `character(1)` A valid assay name in the `SummarizedExperiment`
    of `@molecularProfiles` of a {class_} object for data type `mDataType`.
    @param ... See details.

    @return Accessors: See details.
    @return Setters: An updated `{class_}` object, returned invisibly.
    ",
    ...
)

#' @name CoreSet-accessors
#' @eval .docs_CoreSet_accessors(class_='CoreSet')
NULL


# ======================================
# Accessor Methods
# --------------------------------------


## ====================
## ---- annotation slot

##
## -- annotation

#' @noRd
.docs_CoreSet_get_annotation <- function(...) .parseToRoxygen(
    "
    @details 
    ## @annotation
    __annotation__: A `list` of {class_} annotations with items: 'name', 
    the name of the object; 'dateCreated', date the object was created; 'sessionInfo',
    the `sessionInfo()` when the object was created; 'call', the R constructor call;
    and 'version', the object version.
    @aliases annotation
    @md
    @importMethodsFrom BiocGenerics annotation
    @exportMethod annotation
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_annotation(class_=.local_class)
setMethod('annotation', signature("CoreSet"), function(object) {
    object@annotation
})


#' @noRd
.docs_CoreSet_set_annotation <- function(...) .parseToRoxygen(
    "
    @details 
    __annotation<-__: Setter method for the annotation slot. Arguments:
    - value: a `list` of annotations to update the {class_} with.
    @aliases annotation<-
    @md
    @importMethodsFrom BiocGenerics annotation<-
    @exportMethod annotation<-
    ",
    ...
)

#' @rdname CoreSet-accessors 
#' @eval .docs_CoreSet_set_annotation(class_=.local_class)
setReplaceMethod("annotation", signature("CoreSet", "list"), 
    function(object, value) 
{
    object@annotation <- value
    object
})

##
## -- dateCreated

#' @export
setGeneric("dateCreated", function(object, ...) standardGeneric("dateCreated"))

.docs_CoreSet_get_dateCreated <- function(...) .parseToRoxygen(
    "
    @details
    ## @dateCreated
    __dateCreated__: `character(1)` The date the `{class_}` object was
    created, as returned by the `date()` function.
    @examples
    dateCreated({data_})

    @md
    @aliases dateCreated,{class_}-method dateCreated
    @exportMethod dateCreated
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_dateCreated(class_=.local_class, data_=.local_data)
setMethod('dateCreated', signature("CoreSet"), function(object) {
  object@annotation$dateCreated
})


#' @export
setGeneric("dateCreated<-", function(object, ..., value) 
    standardGeneric("dateCreated<-"))

.docs_CoreSet_set_dateCreated <- function(...) .parseToRoxygen(
    "
    @details
    __dateCreated<-__: Update the 'dateCreated' item in the `annotation` slot of
    a `{class_}` object. Arguments:
    - value: A `character(1)` vector, as returned by the `date()` function.
    @examples
    ## dateCreated
    dateCreated({data_}) <- date()

    @md
    @aliases dateCreated<-,{class_}-method dateCreated<-
    @exportMethod dateCreated<-
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_dateCreated(class_=.local_class, data_=.local_data)
setReplaceMethod('dateCreated', signature(object="CoreSet", value="character"), 
    function(object, value) 
{
  ## TODO:: Error handling - do we ever want to allow a datetime object?
  funContext <- .funContext('dateCreated')
  if (length(value) > 1) .error(funContext, 'dateCreated must by a character
      vector of length 1, as returned by the `date()` function.')
  object@annotation$dateCreated <- value
  return(object)
})


## ==============
## ---- cell slot


##
## -- cellInfo


#' @export
setGeneric("cellInfo", function(object, ...) standardGeneric("cellInfo"))

#' @noRd
.docs_CoreSet_get_cellInfo <- function(...) .parseToRoxygen(
    "
    ## @cell
    @details
    __cellInfo__: `data.frame` Metadata for all cell-lines in a `{class_}` object.

    @md
    @aliases cellInfo,{class_}-method cellInfo
    @exportMethod cellInfo
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_cellInfo(class_=.local_class)
setMethod(cellInfo, "CoreSet", function(object){
  object@cell
})

#' @export
setGeneric("cellInfo<-", function(object, value) standardGeneric("cellInfo<-"))

#' @noRd
.docs_CoreSet_set_cellInfo <- function(...) .parseToRoxygen(
    "
    @details
    __cellInfo<-__: assign updated cell-line annotations to the `{class_}` object. Arguments:
    - value: a `data.frame` object.
    @examples
    cellInfo({data_}) <- cellInfo({data_})

    @md
    @aliases cellInfo<-,{class_},data.frame-method cellInfo<-
    @exportMethod cellInfo<-
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_cellInfo(class_=.local_class, data_=.local_data)
setReplaceMethod("cellInfo", signature(object="CoreSet", value="data.frame"), 
    function(object, value)
{
  funContext <- .funContext('::cellInfo')
  if(is.null(rownames(value)))
    .error(funContext, "Please provide the cell_id as rownames for the cell 
        line annotations")
  object@cell <- value
  object
})


##
## -- cellNames


#' @export
setGeneric("cellNames", function(object, ...) standardGeneric("cellNames"))

#' @noRd
.docs_CoreSet_get_cellNames <- function(...) .parseToRoxygen(
    "
    @details
    __cellNames__: `character` Retrieve the rownames of the `data.frame` in 
    the `cell` slot from a {class_} object.
    @examples
    cellNames({data_})

    @md
    @aliases cellName,{class_}-method cellName
    @exportMethod cellNames
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_cellNames(class_=.local_class, data_=.local_data)
setMethod(cellNames, signature("CoreSet"), function(object){
    rownames(cellInfo(object))
})


#' @export
setGeneric("cellNames<-", function(object, ..., value) 
    standardGeneric("cellNames<-"))

#' @noRd
.docs_CoreSet_set_cellNames <- function(...) .parseToRoxygen(
    "
    @details
    __cellNames<-__: assign new rownames to the cellInfo slot `data.frame` for 
    a {class_} object. Arguments:
    - value: `character` vector of rownames for the `cellInfo(object)` `data.frame`.
    @examples
    cellNames({data_}) <- cellNames({data_})

    @md
    @aliases cellNames<-,{class_},list-method cellNames<- 
    @exportMethod cellNames<-
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_cellNames(class_=.local_class, data_=.local_data)
setReplaceMethod("cellNames", signature(object="CoreSet",value="character"), 
  function(object, value){
    ## TODO: does updateCellId also update slots other than cell?
    object <- updateCellId(object, value)
    return(object)
})


## ------------------
## ---- curation slot


##
## -- curation

#' @export
setGeneric("curation", function(object, ...) standardGeneric("curation"))

#' @noRd
.docs_CoreSet_get_curation <- function(...) .parseToRoxygen(
    "
    @details
    ## @curation
    __curation__: A `list` of curated mappings between identifiers in the 
    {class_} object and the original data publication. {details_}
    @examples
    ## curation
    curation({data_})

    @md
    @aliases curation,{class_}-method curation
    @exportMethod curation
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_curation(class_=.local_class, data_=.local_data, 
#' details_="Contains two `data.frame`s, 'cell' with cell-line ids and 
#' 'tissue' with tissue ids.")
setMethod('curation', signature(object="CoreSet"), function(object) {
    object@curation
})

#' @export
setGeneric("curation<-", function(object, ..., value) 
    sstandardGeneric("curation<-"))

#' @noRd
.docs_CoreSet_set_curation <- function(...) .parseToRoxygen(
    "
    @details
    __curation<-__: Update the `curation` slot of a {class_} object. Arugments:
    - value: A `list` of `data.frame`s, one for each type of curated 
    identifier. {details_}
    @examples
    curation({data_}) <- curation({data_})

    @md
    @aliases curation<-,{class_},list-method curation<-
    @exportMethod curation<-
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_curation(class_=.local_class, data_=.local_data, 
#' details_="For a `CoreSet` object the slot should contain tissue and 
#' cell-line id `data.frame`s.")
setReplaceMethod("curation", signature(object="CoreSet", value="list"), 
    function(object, value)
{
    object@curation <- value
    object
})



# ----------------------
## ---- datasetType slot


#
# -- datasetType

#' @export
setGeneric("datasetType", function(object, ...) standardGeneric("datasetType"))

#' @noRd
.docs_CoreSet_get_datasetType <- function(...) .parseToRoxygen(
    "
    @details
    ## datasetType slot
    __datasetType__: `character(1)` The type treatment response in the 
    `sensitivity` slot. Valid values are 'sensitivity', 'perturbation' or 'both'.
    @examples
    datasetType({data_})

    @md
    @aliases datasetType,{class_}-method datasetType
    @exportMethod datasetType
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_datasetType(class_=.local_class, data_=.local_data)
setMethod("datasetType", signature("CoreSet"), function(object) {
    object@datasetType
})


#' @export
setGeneric("datasetType<-",  function(object, value) 
    standardGeneric("datasetType<-"))

#' @noRd
.docs_CoreSet_set_datasetType <- function(...) .parseToRoxygen(
    "
    @details
    __datasetType<-__: Update the datasetType slot of a {class_} object. 
    Arguments:
    - value: A `character(1)` vector with one of 'sensitivity', 'perturbation' 
    or 'both'
    @examples
    datasetType({data_}) <- 'both'

    @md
    @aliases datasetType<-,{class_},character-method
    @export
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_datasetType(class_=.local_class, data_=.local_data)
setReplaceMethod("datasetType", signature(object="CoreSet", value='character'), 
    function(object, value) 
{
    funContext <- .funContext('datasetType,CoreSet,character-method')
    if (length(value) > 1) .error(funContext, 
        'datasetType must be a character vector of length 1.')
    if (!is.element(value, c('sensitivity', 'perturbation', 'both')))
        .error(funContext, 'datasetType must be one of "sensitivity", 
            "perturbation" or "both".')
    object@datasetType <- value
    object
})



## ---------------------------
## ---- molecularProfiles slot


#
# -- molecularProfiles


#' @export
setGeneric("molecularProfiles", function(object, mDataType, assay, ...) 
    standardGeneric("molecularProfiles"))

#' @noRd
.docs_CoreSet_get_molecularProfiles <- function(...) .parseToRoxygen(
    "
    @details
    ## @molecularProfiles
    __molecularProfiles__: Retrieve 

    @md
    @aliases molecularProfiles,{class_}-method molecularProfiles
    @importClassesFrom S4Vectors DataFrame SimpleList
    @importFrom S4Vectors DataFrame
    @importFrom SummarizedExperiment colData assay assayNames
    @exportMethod molecularProfiles
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_molecularProfiles(class_=.local_class, data_=.local_data)
setMethod(molecularProfiles, "CoreSet", function(object, mDataType, assay){
  funConetext <- .funContext('::molecularProlfiles,CoreSet-method')
  ## TODO:: Add an all option that returns a list?
  if(mDataType %in% names(object@molecularProfiles)){
    if (!missing(assay)) {
      if (assay %in% assayNames(object@molecularProfiles[[mDataType]])) {
        return(SummarizedExperiment::assay(object@molecularProfiles[[mDataType]], assay))
      } else {
        error(funContext, (paste('Assay', assay, 'not found in the SummarizedExperiment object!')))
      }
    } else {
      return(SummarizedExperiment::assay(object@molecularProfiles[[mDataType]], 1))
    }
  } else {
    stop(paste0('mDataType ', mDataType, ' not found the object!'))
  }
})

#' @export
setGeneric("molecularProfiles<-", function(object, mDataType, assay, value) 
    standardGeneric("molecularProfiles<-"))

#' @noRd
.docs_CoreSet_set_molecularProfiles <- function(...) .parseToRoxygen(
    "
    @details
    __molecularProfiles<-__: Update an assay in a `SummarizedExperiment` from 
    the `molecularProfiles` slot of a {class_} object with the specified
    `mDataType`. Valid `mDataType` arguments can be found with 
    `mDataNames(object)`.
    - assay: Optional `character(1)` vector specifying an assay in the 
    `SummarizedExperiment` of the `molecularProfiles` slot of the
    `{class_}` object for the specified `mDataType`. If excluded, 
    defaults to modifying the first assay in the `SummarizedExperiment` for
    the given `mDataType`.
    - value: A `matrix` of values to assign to the `assay` slot of the 
    `SummarizedExperiment` for the selected `mDataType`. The rownames and 
    column names must match the associated `SummarizedExperiment`.
    @examples
    # No assay specified
    molecularProfiles({data_}, 'rna') <- molecularProfiles({data_}, 'rna')

    # Specific assay
    molecularProfiles({data_}, 'rna', 'exprs') <- molecularProfiles({data_}, 'rna', 'exprs')

    @md
    @aliases molecularProfiles<-,{class_},character,character,matrix-method 
    molecularProfiles<-,{class_},character,missing,matrix-method  
    molecularProfiles<-
    @importFrom SummarizedExperiment assay
    @exportMethod molecularProfiles<-
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_molecularProfiles(class_=.local_class, data_=.local_data)
setReplaceMethod("molecularProfiles", signature(object="CoreSet", 
    mDataType ="character", assay="character", value="matrix"),
    function(object, mDataType, assay, value)
{
  if (mDataType %in% names(object@molecularProfiles)) {
    SummarizedExperiment::assay(object@molecularProfiles[[mDataType]], assay) <- value
  }
  object
})
setReplaceMethod("molecularProfiles",
                 signature(object="CoreSet", mDataType ="character",
                           assay="missing", value="matrix"),
                 function(object, mDataType, assay, value) {
  if (mDataType %in% names(object@molecularProfiles)) {
    SummarizedExperiment::assay(object@molecularProfiles[[mDataType]], 1) <- value
  }
  object
})


## -- featureInfo

#' @export
setGeneric("featureInfo", function(object, mDataType, ...) 
    standardGeneric("featureInfo"))

#' @noRd
.docs_CoreSet_get_featureInfo <- function(...) .parseToRoxygen(
    "
    @details
    __featureInfo__: Retrieve a `DataFrame` of feature metadata for the specified
    `mDataType` from the `molecularProfiles` slot of a {class_} object.
    @examples
    featureInfo({data_}, 'rna')

    @md
    @aliases featureInfo,{class_}-method featureInfo
    @importFrom SummarizedExperiment rowData rowData<-
    @exportMethod featureInfo
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_featureInfo(class_=.local_class, data_=.local_data)
setMethod(featureInfo, "CoreSet", function(object, mDataType) {
  if (mDataType %in% names(object@molecularProfiles)) {
    return(rowData(object@molecularProfiles[[mDataType]]))
  } else{
    return(NULL)
  }
})

#' @noRd
.docs_CoreSet_set_featureInfo <- function(...) .parseToRoxygen(
    "
    @details
    __featureInfo<-__: Update the `featureInfo(object, mDataType)` `DataFrame`
    with new feature metadata. Arguments:
    - value: A `data.frame` or `DataFrame` with updated feature metadata for
    the specified molecular profile in the `molecularProfiles` slot of a 
    `{class_}` object.
    @examples
    featureInfo({data_}) <- featureInfo({data_})
    
    @aliases featureInfo<-,{class_},character,data.frame-method 
    featureInfo<-,{class_},character,DataFrame-method featureInfo<-
    @importFrom SummarizedExperiment rowData rowData<-
    @importFrom S4Vectors DataFrame
    @exportMethod featureInfo<-
    ",
    ...
)

#' @export
setGeneric("featureInfo<-", function(object, mDataType, value) 
    standardGeneric("featureInfo<-"))

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_featureInfo(class_=.local_class, data_=.local_data)
setReplaceMethod("featureInfo", signature(object="CoreSet", 
    mDataType ="character",value="data.frame"), 
    function(object, mDataType, value)
{

  if(mDataType %in% names(object@molecularProfiles)){
    rowData(object@molecularProfiles[[mDataType]]) <-
      S4Vectors::DataFrame(value, rownames = rownames(value))
  }
  object
})
setReplaceMethod("featureInfo", signature(object="CoreSet", 
    mDataType ="character",value="DataFrame"), 
    function(object, mDataType, value)
{
  if(mDataType %in% names(object@molecularProfiles)){
    rowData(object@molecularProfiles[[mDataType]]) <-
      S4Vectors::DataFrame(value, rownames = rownames(value))
  }
  object
})

## -- mDataNames

#' @export
setGeneric("mDataNames", function(object, ...) standardGeneric("mDataNames"))

#' @noRd
.docs_CoreSet_get_mDataNames <- function(...) .parseToRoxygen(
    "
    @details
    __mDataNames__: `character` Retrieve the names of the molecular data types
    available in the `molecularProfiles` slot of a `{class_}` object. These
    are the options which can be used in the `mDataType` parameter of various
    `molecularProfiles` slot accessors methods.
    @examples
    mDataNames({data_})

    @md
    @exportMethod mDataNames
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_mDataNames(class_=.local_class, data_=.local_data)
setMethod("mDataNames", "CoreSet", function(object){
  return(names(object@molecularProfiles))
})


#' @export
setGeneric("mDataNames<-", function(object, ..., value) standardGeneric("mDataNames<-"))

#' @noRd
.docs_CoreSet_set_mDataNames <- function(...) .parseToRoxygen(
    "
    @details
    __mDataNames__: Update the molecular data type names of the 
    `molecularProfiles` slot of a {class_} object. Arguments:
    - value: `character` vector of molecular datatype names, with length
    equal to `length(molecularProfilesSlot(object))`.
    @examples
    mDataNames({data_}) <- mDataNames({data_})

    @md
    @aliases mDataNames<-,{class_},ANY-method mDataNames
    @exportMethod mDataNames<-
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_mDataNames(class_=.local_class, data_=.local_data)
setReplaceMethod("mDataNames", "CoreSet", function(object, value){
  names(object@molecularProfiles) <- value
  return(object)
})


## ---------------------
## ---- sensitivity slot




#
# -- sensitivityInfo

#' @noRd
.docs_CoreSet_get_sensitivityInfo <- function(...) .parseToRoxygen(
    "
    @details
    ## @sensitivity

    Arguments:
    - `dimension`: Optional `character(1)` One of 'drug', 'cell' or 'assay' to
    retrieve `rowData`, `colData` or the 'assay_metadata' assay from the 
    `{class_}` `@sensitvity` `LongTable` object, respectively. Ignored with 
    warning if `@sensitivity` is not a `LongTable` object.
    -  `...`: Additional arguments to the `rowData` or `colData`.
    `LongTable` methods. Only used if the sensitivity slot contains a
    `LongTable` object instead of a `list` and the `dimension` argument is 
    specified.

    __sensitivityInfo__: `DataFrame` or `data.frame` of sensitivity drug combo
    by cell-line metadata for the `{class_}` object. When the `dimension` 
    parameter is used, it allows retrieval of the dimension specific metadata 
    from the `LongTable` object in `@sensitivity` of a {class_} object.

    @examples
    sensitivityInfo({data_})

    @md
    @aliases sensitivityInfo,{class_},missing-method 
    sensitivityInfo,{class_},character-method sensitivityInfo
    @exportMethod sensitivityInfo
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_sensitivityInfo(class_=.local_class, data_=.local_data)
setMethod(sensitivityInfo, signature("CoreSet"),
    function(object, dimension, ...) 
{
    funContext <- .funContext('::sensitivityInfo')
    # case where sensitivity slot is a LongTable
    if (is(sensitivitySlot(object), 'LongTable')) {
        if (!missing(dimension)) {
            switch(dimension,
                cell={ return(colData(sensitivitySlot(object), ...)) },
                drug={ return(rowData(sensitivitySlot(object), ...)) },
                assay={ return(assay(sensitivitySlot(object), 'assay_metadata')) },
                .error(funContext, 'Invalid value for the dimension argument. 
                    Please select on of "cells", "drugs" or "assays'))
        } else {
            return(.rebuildInfo(sensitivitySlot(object)))
        }
    # sensitivity is a list
    } else {
        if (!missing(dimension))
            .warning(funContext,' The dimension argument is only valid if the 
                sensitivity slot contains a LongTable object. Ignoring the 
                dimension and ... parameters.')
        return(sensitivitySlot(object)$info)
    }
})


#' Replicate the $info slot in the old sensitivity list from the new LongTable
#'   object
#'
#' @param longTable [`LongTable`]
#'
#' @keywords internal
#' @importFrom MatrixGenerics colAlls
#' @importFrom data.table setkeyv merge.data.table `:=` setDF
#' @noRd
.rebuildInfo <- function(longTable) {

    # Extract the information needed to reconstruct the sensitivityInfo data.frame
    assayIndexDT <- assay(longTable, 1, key=TRUE)[, .(rowKey, colKey)]
    setkeyv(assayIndexDT, c('rowKey', 'colKey'))
    rowDataDT <- rowData(longTable, key=TRUE)
    setkeyv(rowDataDT, 'rowKey')
    colDataDT <- colData(longTable, key=TRUE)
    setkeyv(colDataDT, 'colKey')

    rowIDcols <- rowIDs(longTable)[!grepl('dose', rowIDs(longTable))]
    colIDcols <- colIDs(longTable)
    rownameCols <- c(rowIDcols, colIDcols)

    # join the tables into the original data
    infoDT <- merge.data.table(assayIndexDT, rowDataDT, all=TRUE)
    setkeyv(infoDT, 'colKey')
    infoDT <- merge.data.table(infoDT, colDataDT, all=TRUE)[, -c('rowKey', 'colKey')]

    # determine which columns map 1:1 with new identifiers and subset to those
    infoDT_first <- infoDT[, head(.SD, 1), by=rownameCols]
    infoDT_last <- infoDT[, tail(.SD, 1), by=rownameCols]
    keepCols <- names(which(apply(infoDT_first == infoDT_last, MARGIN=2, 
        FUN=all)))
    infoDT_sub <- unique(infoDT[, ..keepCols])

    # rebuld the rownames
    .paste_ <- function(x, y) paste(x, y, sep='_')
    .paste_colon <- function(x, y) paste(x, y, sep=':')
    infoDT_sub[, drugid := Reduce(.paste_colon, mget(..rowIDcols))]
    infoDT_sub[, cellid := Reduce(.paste_colon, mget(..colIDcols))]
    infoDT_sub[, exp_id := Reduce(.paste_, .(drugid, cellid))]

    # convert to data.frame
    setDF(infoDT_sub, rownames=infoDT_sub$exp_id)
    return(infoDT_sub)
}

#' @noRd
.docs_CoreSet_set_sensitivityInfo <- function(...) .parseToRoxygen(
    "
    @details
    __sensitivityInfo__<-: Update the `@sensitivity` slot metadata for a 
    `{class_}` object. When used without the `dimension` argument is behaves
    similar to the old {class_} implementation, where the `@sensitivity` slot
    contained a list with a `$info` `data.frame` item. When the `dimension`
    arugment is used, more complicated assignments can occur where 'cell' 
    modifies the `@sensitvity` `LongTable` colData, 'drug' the rowData and
    'assay' the 'assay_metadata' assay.
    Arguments:
    - value: A `data.frame` of treatment response experiment metadata, 
    documenting experiment level metadata (mapping to drugs and cells). If
    the `@sensitivity` slot doesn't contain a `LongTable` and `dimension` is
    not specified, you can only modify existing columns as returned by 
    `sensitivityInfo(object)`.
    @examples
    sensitivityInfo({data_}) <- sensitivityInfo({data_})

    @md
    @aliases sensitivityInfo<-,{class_},missing,data.frame-method 
    sensitviityInfo<-,{class_},character,data.frame-method sensitivityInfo<-
    @import data.table
    @exportMethod sensitivityInfo<-
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_sensitivityInfo(class_=.local_class, data_=.local_data)
setReplaceMethod("sensitivityInfo", signature(object="CoreSet", value="data.frame"),
                
                function(object, dimension, ..., value) {

    funContext <- funContext('::sensitivityInfo<-')
    if (is(sensitivitySlot(object), 'LongTable')) {
        # coerce to data.table
        if (!is.data.table(value)) value <- data.table(value, keep.rownames=TRUE)
        if (missing(dimension)) {
            valueCols <- colnames(value)
            # get existing column names
            rowDataCols <- colnames(rowData(object@sensitivity))
            colDataCols <- colnames(colData(object@sensitivity))
            # drop any value columns that don't already exist
            hasValueCols <- valueCols %in% c(rowDataCols, colDataCols)
            if (!all(hasValueCols))
                .warning(funContext, 'Dropping columns ', 
                    .collapse(valueCols[!hasValueCols]), ' from value. Currently 
                    this setter only allows modifying existing columns when 
                    @sensitivity is a LongTable. For more fine grained updates 
                    please use the dimension argument.')
            # update the object
            rowData(object@sensitivity, ...) <-
                unique(value[, .SD, .SDcols=valueCols %in% rowDataCols])
            colData(object@sensitivity, ...) <-
                unique(value[, .SD, .SDcols=valueCols %in% colDataCols])
        } else {
            switch(dimension,
                drug={ rowData(object@sensitivity, ...) <- value },
                cell={ colData(object@sensitivity, ...) <- value },
                assay={ assay(object@sensitivity, 'assay_metadata') <- value },
                .error(funContext, 'Invalid argument to dimension parameter. 
                    Please choose one of "cells" or "drugsA"'))
        }
    } else {
        if (!missing(dimension))
            .warning(funContext, 'The dimension argument is only valid if the 
                sensitivity slot contains a LongTable object. Ignoring dimension 
                and ... parameters.')
        object@sensitivity$info <- value
    }
    return(object)
})

#
# -- sensitvityMeasures

#' @noRd
.docs_CoreSet_get_sensitivityMeasures <- function(...) .parseToRoxygen(
    "
    @details
    __sensitivityMeaures__: Get the 'sensitivityMeasures' available in a `{class_}`
    object. Each measure reprents some summary of cell-line sensitivity to a given
    dryuh, such as ic50, ec50, AUC, AAC, etc. The results are returned as a 
    `character` vector with all available metrics for the PSet object.
    @examples
    sensitivityMeasures({data_}) <- sensitivityMeasures({data_})

    @md
    @exportMethod sensitivityMeasures
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_sensitivityMeasures(class_=.local_class, data_=.local_data)
setMethod(sensitivityMeasures, "CoreSet", function(object){
    return(colnames(sensitivityProfiles(object)))
})

#' @noRd
.docs_CoreSet_set_sensitityMeasures <- function(...) .parseToRoxygen(
    "
    @details
    __sensitivityMeaures__: Update the sensitivity meaure in a `{class_}`
    object. Thesee values are the column names of the 'profiles' assay and 
    represent various compued sensitviity metrics such as ic50, ec50, AUC, AAC,
    etc.
    - value: A `character` vector of new sensitivity measure names, the
    then length of the character vector must matcht he number of columns of the
    'profiles' assay, excluding metadata and key columns.
    @examples
    sensitivityMeasures({data_}) <- sensitivityMeasures({data_})

    @md
    @exportMethod sensitivityMeasures
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_sensitityMeasures(class_=.local_class, data_=.local_data) 
setReplaceMethod('sensitivityMeasures',
    signature(object='CoreSet', value='character'),
    function(object, value) {
  colnames(sensitivityProfiles(object)) <- value
  object
})

#
# -- sensitivityProfiles

#' @noRd
.docs_CoreSet_get_sensitivityProfiles <- function(...) .parseToRoxygen(
    "
    @details
    __sensitivityProfiles__: Return the sensitivity profile summaries from the
    sensitivity slot. This data.frame cotanins vaarious sensitivity summary
    metrics, such as ic50, amax, EC50, aac, HS, etc as columns, with rows as 
    drug by sample experiments.
    @examples
    sensitivityProfiles({data_})

    @md
    @exportMethod sensitivityInfo
    ",
    ...
)


#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_sensitivityProfiles(class_=.local_class, data_=.local_data)
setMethod(sensitivityProfiles, "CoreSet", function(object) {
    funContext <- .funContext('::sensitivityProfiles')
    if (is(sensitivitySlot(object), 'LongTable')) {
        if (!('profiles' %in% assayNames(sensitivitySlot(object))))
            .error(funContext, 'The LongTable onject in the sensivitiy slot
                is not formatted correctly: it must contain and assay
                named "profiles"!')
    } else {
        return(object@sensitivity$profiles)
    }
})

#' @noRd
.docs_CoreSet_set_sensitivityProfiles <- function(...) .parseToRoxygen(
    "
    @details
    __sensitivityProfiles<-__: Update the sensitivity profile summaries the 
    sensitivity slot. Arguments:
    -value: A `data.frame` the the same number of rows as as returned by
    `sensitivityInfo(object)`, but potentially modified columns, such as the 
    computation of additional summary metrics.
    @examples
    sensitivityInfo({data_} <- sensitivityInfo({data_})

    @md
    @exportMethod sensitivityProfiles<-
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_sensitivityProfiles(class_=.local_class, data_=.local_data)
setReplaceMethod("sensitivityProfiles",
                 signature = signature(object="CoreSet",
                                       value="data.frame"),
                 function(object, value) {
    if (is(sensitivitySlot(object), 'LongTable'))
        stop(.errorMsg('[CoreGx::sensitivityProfiles<-] No setter has been ',
            'implemented for this method when the sensitivity slot in a CoreSet',
            ' is a LongTable. Please define a method using setReplaceMethod() ',
            'for the subclass of CoreSet in your current package!'))
    else
        object@sensitivity$profiles <- value
    return(object)
})
#' @describeIn CoreSet Update the phenotypic data for the drug dose
#'   sensitivity
#'
#' @export
setReplaceMethod("sensitivityProfiles",
                 signature = signature(object="CoreSet",
                                       value="matrix"),
                function(object, value) {
    if (is(sensitivitySlot(object), 'LongTable'))
        stop(.errorMsg('[CoreGx::sensitivityProfiles<-] No setter has been ',
            'implemented for this method when the sensitivity slot in a CoreSet',
            ' is a LongTable. Please define a method using setReplaceMethod() ',
            'for the subclass of CoreSet in your current package!'))
    else
        object@sensitivity$profiles <- as.data.frame(value)
    return(object)
})


#
# -- sensitivityRaw

#' sensitivityRaw CoreSet Getter Method
#'
#' @describeIn CoreSet Get the raw dose and vaibility data from a CoreSet object.
#'
#' @examples
#' data(clevelandSmall_cSet)

#'
#' @param object A [`CoreSet`] to extract the raw sensitivity data from
#'
#' @return A [`array`] containing the raw sensitivity data as experiment by
#'     dose level by metric.
#'
#' @export
#' @noRd
.docs_CoreSet_get_sensitivityRaw <- function(...) .parseToRoxygen(
    "
    @details
    __sensitivityRaw__: Access the raw sensitiity measumrents for a {class_}. 
    Returns a 3D array
    @examples
    head(sensitivityRaw(clevelandSmall_cSet{data_}))
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_sensitivityRaw(class_=.local_class, data_=.local_data)
#' @exportMethod sensitivityRaw
setMethod("sensitivityRaw", signature("CoreSet"), function(object) {
    if (is(sensitivitySlot(object), 'LongTable'))
        return(.rebuildRaw(sensitivitySlot(object)))
    else
        return(object@sensitivity$raw)
})

#' Replicate the $raw slot in the old @sensitivity list from a LongTable
#'
#' @param longTable [`LongTable`]
#'
#' @return A 3D [`array`]
#'
#' @keywords internal
#' @noRd
.rebuildRaw <- function(longTable) {

    # Extract the information needed to reconstruct the sensitivityRaw array
    viability <- longTable$sensitivity
    viability[, rownames := paste0(
        paste0(mget(rowIDs(longTable)), collapse=':'), '_',
        paste0(mget(colIDs(longTable)), collapse=':'))]


    # Join to a single wide data.table
    arrayData <- meta[dose][viab][, -c('rowKey', 'colKey')]

    # Get the data matrices
    Dose <- as.matrix(arrayData[, grep('Dose', colnames(arrayData), value=TRUE), with=FALSE])
    Viability <- as.matrix(arrayData[, grep('Viability', colnames(arrayData), value=TRUE), with=FALSE])

    array <- simplify2array(list(Dose, Viability))
    dimnames(array) <- list(arrayData$rn,
                            paste0('doses', seq_len(ncol(Dose))),
                            c('Dose', 'Viability'))
    return(array)
}

## TODO:: Make this a unit test
## TEST: all.equal(raw[dimnames(SR)[[1]],,], SR)

.docs_CoreSet_set_sensitivityRaw <- function(...) .paresToRoxygen(
    "
    @details
    __sensitviityRaw<-__: Update the raw dose and viability data in a pSet object.
    - value: 

    @md
    @exportMethod sensitivityRaw<-
    "
    ,
    ...
)

#' sensitivityRaw<- Setter
#'
#' @describeIn PharmacoSet
#'
#' @examples
#' data(CCLEsmall)
#' sensitivityRaw(CCLEsmall) <- sensitivityRaw(CCLEsmall)
#'
#' @param object A \code{PharmacoSet} to extract the raw sensitivity data from
#' @param value A \code{array} containing the raw dose and viability data for the
#'   pSet
#'
#' @return A copy of the \code{PharmacoSet} containing the updated sensitivty data
#'
#' @import data.table
#'
#' @export
setReplaceMethod('sensitivityRaw', signature("CoreSet", "array"), 
    function(object, value) 
{
    if (is(sensitivitySlot(object), 'LongTable')) {

        ## TODO:: validate value

        longTable <- sensitivitySlot(object)

        raw <- as.data.table(value, keep.rownames=TRUE, na.rm=FALSE)

        # preprocess raw array
        ## FIXME:: refactor this into a helper, it is repeated in sensitivtySlotToLongTable
        setnames(raw, seq_len(3), c('rn', 'replicate', 'assay'))
        assayIDs <- unique(raw$assay)
        raw[, value := as.numeric(value)]
        raw[, replicate := as.integer(gsub('\\D*', '', replicate))]
        # Split value into one column for each assay (long -> wide)
        longRaw <- dcast(raw, rn + replicate ~ ..., value.var=c('value'))
        # Split assay columns into assay per replicate (wide -> wider)
        longRaw <- dcast(longRaw, rn ~ replicate, value.var=assayIDs)

        assayData <- assays(longTable, withDimnames=TRUE, key=FALSE)
        expMetadata <- assayData$experiment_metadata[, c(idCols(longTable), 'rn'), with=FALSE]
        rawData <- merge.data.table(longRaw, expMetadata, by='rn')

        doseCols <- grep('Dose_\\d+', colnames(rawData), value=TRUE)
        assayData$dose <-
            rawData[, c(idCols(longTable), doseCols), with=FALSE]
        viabilityCols <- grep('Viability_\\d+', colnames(rawData), value=TRUE)
        assayData$viability <-
            rawData[, c(idCols(longTable), viabilityCols), with=FALSE]

        assays(longTable) <- assayData
        object@sensitivity <- longTable

    } else {
        funContext <- funContext("::sensitivityRaw<-")
        if (!is.array(value)) .error(funContexnt, "Values assigned to the
            sensitivityRaw must be a matrix of experiment by dose by value!")
        object@sensitivity$raw <- value
        object
    }
    return(object)
})


#
# -- sensitivitySlot

#' sensitivitySlot Generic
#'
#' @examples
#' data(clevelandSmall_cSet)
#' sensitivitySlot(clevelandSmall_cSet)
#'
#' @param object A \code{CoreSet} to extract the raw sensitivity data from
#' @param ... A \code{list} to allow new parameters in specific methods
#'
#' @return A \code{list} of the sensitivity slot contents
#'
#' @export
setGeneric("sensitivitySlot", function(object, ...) standardGeneric("sensitivitySlot"))
#' @describeIn CoreSet Retrieve the contents of the sensitivity slot
#' @inheritParams sensitivitySlot
#' @export
setMethod("sensitivitySlot", signature("CoreSet"), function(object) {
  object@sensitivity
})

##TODO:: Migrate this to CoreGx
#' sensitivitySlot<- Replacement Generic
#'
#' @examples
#' data(clevelandSmall_cSet)
#' sensitivitySlot(clevelandSmall_cSet) <- sensitivitySlot(clevelandSmall_cSet)
#'
#' @param object A \code{CoreSet} to extract the raw sensitivity data from
#' @param ... A \code{list} to allow new parameters in specific methods
#' @param value A \code{list} of new sensitivity slot data for the rSet
#'
#' @return A copy of the \code{CoreSet} containing the updated sensitivty slot
#'
#' @export
setGeneric("sensitivitySlot<-", function(object, ..., value) standardGeneric("sensitivitySlot<-"))
#' @describeIn CoreSet Set the raw dose and viability data for a cSet and return
#'   and updated copty
#' @inheritParams sensitivitySlot<-
#' @export
setReplaceMethod("sensitivitySlot", signature("CoreSet", "list"),
                 function(object, value) {
                   ##TODO:: Implement error handling for this slot
                   object@sensitivity <- value
                   object
                 })