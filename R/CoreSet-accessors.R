#' @include CoreSet-class.R
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
    @param assay See details.
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
    ## annotation slot
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
    ## dateCreated slot
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
setGeneric("dateCreated<-", function(object, ..., value) standardGeneric("dateCreated<-"))

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
    ## cell slot
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
  if(is.null(rownames(value)))
    stop("Please provide the cell_id as rownames for the cell line annotations")
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
setGeneric("cellNames<-", function(object, ..., value) standardGeneric("cellNames<-"))

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
    ## curation slot
    __curation__: A `list` of curated mappings between identifiers in the 
    {class_} object and the original data publication. {details_}
    @examples
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
setGeneric("curation<-", function(object, ..., value) standardGeneric("curation<-"))

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
    ## molecularProfiles slot
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
  ## TODO:: Add an all option that returns a list?
  if(mDataType %in% names(object@molecularProfiles)){
    if (!missing(assay)) {
      if (assay %in% assayNames(object@molecularProfiles[[mDataType]])) {
        return(SummarizedExperiment::assay(object@molecularProfiles[[mDataType]], assay))
      } else {
        stop(paste('Assay', assay, 'not found in the SummarizedExperiment object!'))
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
  if (mDataType %in% names(object@molecularProfiles)){
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