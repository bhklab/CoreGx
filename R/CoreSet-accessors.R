#' @include CoreSet-class.R
NULL

# =======================================
# Accessor Method Documentation Object
# ---------------------------------------

#' @name CoreSet-accessors
#' @title Accessing and modifying information in `CoreSet`
#'
#' @details
#' Documentation for the various setters and getters which allow manipulation
#' of data in the slots of a `CoreSet` object.
#'
#' @param object A `CoreSet` object.
#' @param value See details.
#' @param ... See details.
#'
#' @return Accessors:
#' - cellInfo
#'
#' @return Setters: An updated `CoreSet` object, returned invisibly.
#'
#' @md
#' @family CoreSet-accessors
#' @seealso [`CoreSet-class`]
NULL

# ======================================
# Accessor Methods
# --------------------------------------

## ---- annotation slot

#' @rdname CoreSet-accessors
#' @details annotation: getter method for annotation slot.
#'
#' @include CoreSet-class.R
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
#' annotation<-: setter method for the annotation slot.
#' - parameters:
#'   - value: a `list` of annotations to update the `CoreSet` with.
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

# ---- cellInfo slot

#' @export
setGeneric("cellInfo", function(object, ...) standardGeneric("cellInfo"))

#' @rdname CoreSet-accessors
#' @details
#' 
#'
#' @aliases cellInfo,CoreSet-method
#' @exportMethod cellInfo
setMethod(cellInfo, "CoreSet", function(object){
  object@cell
})

#' @export
setGeneric("cellInfo<-", function(object, value) standardGeneric("cellInfo<-"))

#' @rdname CoreSet-accessors
#' @details
#' cellInfo<-: assign updated cell-line annotations to the `r .local_class` object
#' - parameters:
#    - value: a `data.frame` object.
#'
#' @aliases cellInfo<-,CoreSet,data.frame-method
#' @exportMethod cellInfo<-
setReplaceMethod("cellInfo", signature(object="CoreSet", value="data.frame"), 
    function(object, value)
{
  if(is.null(rownames(value)))
    stop("Please provide the cell_id as rownames for the cell line annotations")
  object@cell <- value
  object
})