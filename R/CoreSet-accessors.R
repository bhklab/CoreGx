#' @include CoreSet-class.R
NULL

.local_class <- 'CoreSet'
data('clevelandSmall_cSet')

# =======================================
# Accessor Method Documentation Object
# ---------------------------------------

#' @noRd
#' @keywords internal
.docs_CoreSet_accessors <- function() {
    paste0(c("",
"@details",
"Documentation for the various setters and getters which allow manipulation",
"of data in the slots of a ``r .local_class`` object:",
"@param object A ``r .local_class`` object.",
"@param value See details.",
"@param ... See details.",
"@return Accessors: See details.",
"",
"@return Setters: An updated ``r .local_class`` object, returned invisibly.",
"@md\n"), collapse="\n#' ")}

#' @title Accessing and modifying information in a ``r .local_class``
#' @name CoreSet-accessors
#' @md
cat(.docs_CoreSet_accessors())
`CoreSet-accessors` <- NULL


# ======================================
# Accessor Methods
# --------------------------------------


## --------------------
## ---- annotation slot


#' @rdname CoreSet-accessors
#' @details 
#' __annotation__: A `list` of ``r .local_class`` annotations with items: 'name', 
#' the name of the object; dateCreated, date the object was created; 'sessionInfo',
#' the `sessionInfo()` when the object was created; 'call', the R constructor call;
#' and 'version', the object version.
#'
#' @aliases annotation
#'
#' @md
#' @importMethodsFrom BiocGenerics annotation
#' @exportMethod annotation
setMethod('annotation', signature("CoreSet"), function(object) {
    object@annotation
})

#' @rdname CoreSet-accessors 
#' @details 
#' __annotation<-__: Setter method for the annotation slot. Arguments:
#' - value: a `list` of annotations to update the `r .local_class` with.
#'
#' @aliases annotation<-
#'
#' @md
#' @importMethodsFrom BiocGenerics annotation<-
#' @exportMethod annotation<-
setReplaceMethod("annotation", signature("CoreSet", "list"), function(object, value) {
    object@annotation <- value
    object
})


#' @export
setGeneric("dateCreated", function(object, ...) standardGeneric("dateCreated"))

#' @rdname CoreSet-accessors
#' @details
#' __dateCreated__: `character(1)` The date the ``r .local_class`` object was
#' created, as returned by the `date()` function.
#'
#' @examples
#' dateCreated(clevelandSmall_cSet)
#'
#' @md
#' @aliases dateCreated,CoreSet-method dateCreated
#' @exportMethod dateCreated
setMethod('dateCreated', signature("CoreSet"), function(object) {
  object@annotation$dateCreated
})

#' @export
setGeneric("dateCreated<-", function(object, ..., value) standardGeneric("dateCreated<-"))

#' @rdname CoreSet-accessors
#' @details
#' __dateCreated<-__: Update the 'dateCreated' item in the `annotation` slot of
#' a ``r .local_class`` object. Arguments:
#' - value: A `character(1)` vector, as returned by the `date()` function.
#'
#' @examples
#' dateCreated(clevelandSmall_cSet)
#'
#' @md
#' @aliases dateCreated<-,CoreSet-method dateCreated<-
#' @exportMethod dateCreated<-
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

#' dateCreated<- Generic
#'
#' A generic for the dateCreated method
#'
#' @examples
#' dateCreated(clevelandSmall_cSet) <- date()
#'
#' @param object A \code{CoreSet}
#' @param ... Fallthrough arguements for defining new methods
#' @param value A \code{datetime} object to update the cSet with
#'
#' @return The date the CoreSet was created

## ------------------
## ---- cellInfo slot


#' @export
setGeneric("cellInfo", function(object, ...) standardGeneric("cellInfo"))

#' @rdname CoreSet-accessors
#' @details
#' __cellInfo__: `data.frame` Metadata for all cell-lines in a ``r .local_class`` object.
#' 
#' @md
#' @aliases cellInfo,CoreSet-method cellInfo
#' @exportMethod cellInfo
setMethod(cellInfo, "CoreSet", function(object){
  object@cell
})


#' @export
setGeneric("cellInfo<-", function(object, value) standardGeneric("cellInfo<-"))

#' @rdname CoreSet-accessors
#' @details
#' __cellInfo<-__: assign updated cell-line annotations to the ``r .local_class`` object. Arguments:
#  - value: a `data.frame` object.
#'
#' @examples
#' cellInfo <- cellInfo(clevelandSmall_cSet)
#'
#' @md
#' @aliases cellInfo<-,CoreSet,data.frame-method cellInfo<-
#' @exportMethod cellInfo<-
setReplaceMethod("cellInfo", signature(object="CoreSet", value="data.frame"), 
    function(object, value)
{
  if(is.null(rownames(value)))
    stop("Please provide the cell_id as rownames for the cell line annotations")
  object@cell <- value
  object
})

#' @export
setGeneric("cellNames", function(object, ...) standardGeneric("cellNames"))

#' @rdname CoreSet-accessors
#' @details
#' __cellNames__: `character` Retrieve the rownames of the cellInfo slot from a ``r .local_class`` object.
#'
#' @examples
#' cellNames(clevelandSmall_cSet)
#' 
#' @md
#' @aliases cellName,CoreSet-method cellName
#' @exportMethod cellNames
setMethod(cellNames, signature("CoreSet"), function(object){
  rownames(cellInfo(object))
})

#' @export
setGeneric("cellNames<-", function(object, ..., value) standardGeneric("cellNames<-"))

#' @rdname CoreSet-accessors
#' @details
#' __cellNames<-__: assign new rownames to the cellInfo slot `data.frame` for a ``r .local_class`` object. Arguments:
#' - value: 
#'
#' @examples
#' cellNames(clevelandSmall_cSet) <- cellNames(clevelandSmall_cSet)
#'
#' @md
#' @aliases cellNames<-,CoreSet,list-method cellNames<- 
#' @exportMethod cellNames<-
setReplaceMethod("cellNames", signature(object="CoreSet",value="character"), 
function(object, value){
    object <- updateCellId(object, value)
    return(object)
})


## ------------------
## ---- curation slot


#' @export
setGeneric("curation", function(object, ...) standardGeneric("curation"))

#' @rdname CoreSet-accessors
#' @details
#' __curation__: A `list` of curated mappings between identifiers in the 
#' ``r .local_class`` object and the original data publication. Contains two
#' `data.frame`s, 'cell' with cell-line ids and 'tissue' with tissue ids.
#'
#' @examples
#' curation(clevelandSmall_cSet)
#'
#' @md
#' @aliases curation,CoreSet-method curation
#' @exportMethod curation
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

