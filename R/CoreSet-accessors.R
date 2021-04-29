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
    @details
    Documentation for the various setters and getters which allow manipulation
    of data in the slots of a `{class_}` object:
    @param object A `{class_}` object.
    @param value See details.
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
    __cellNames__: `character` Retrieve the rownames of the cellInfo slot from a {class_} object.
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

.docs_CoreSet_get_curation <- function(...) .parseToRoxygen(
    "
    @details
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

#' @rdname CoreSet-accessors
#' @details
#' __curation<-__: Update the `curation` slot of a ``r .local_class`` object. Arugments:
#' - value: A `list` of `data.frame`s, one for each type of curated identifier. For a ``r .local_class`` object the slot should contain tissue and cell-line id `data.frame`s.
#'
#' @examples
#' curation(clevelandSmall_cSet) <- curation(clevelandSmall_cSet)
#'
#' @md
#' @aliases curation<-,CoreSet,list-method curation<-
#' @exportMethod curation<-
setReplaceMethod("curation", signature(object="CoreSet", value="list"), 
    function(object, value)
{
    object@curation <- value
    object
})



# ----------------------
## ---- datasetType slot


#' @export
setGeneric("datasetType", function(object, ...) standardGeneric("datasetType"))

#' @rdname CoreSet-accessors
#' @details
#' __datasetType__: `character(1)` The type treatment response in the 
#' `sensitivity` slot. Valid values are 'sensitivity', 'perturbation' or 'both'.
#'
#' @examples
#' datasetType(clevelandSmall_cSet)
#'
#' @md
#' @aliases datasetType,CoreSet-method datasetType
#' @exportMethod datasetType
setMethod("datasetType", signature("CoreSet"), function(object) {
    object@datasetType
})


#' @export
setGeneric("datasetType<-",  function(object, value) standardGeneric("datasetType<-"))

#' @rdname CoreSet-accessors
#' @details
#' __datasetType<-__: Update the datasetType slot of a ``r .local_class`` object. Arguments:
#' - value: A `character(1)` vector with 
#' 
#' @examples
#' datasetType(clevelandSmall_cSet) <- 'both'
#'
#' @md
#' @aliases datasetType<-,CoreSet,character-method
#' @export
setReplaceMethod("datasetType", signature(object="CoreSet", value='character'), 
    function(object, value) 
{
    funContext <- .funContext('datasetType,CoreSet,character-method')
    if (length(value) > 1) .error(funContext, 
        'datasetType must be a character vector of length 1.')
    if (!is.element(value, c('sensitivity', 'perturbation', 'both')))
        .error(funContex, 'datasetType must be one of "sensitivity", 
            "perturbation" or "both".')
    object@datasetType <- value
    object
})

## ---------------------------
## ---- molecularProfiles slot

