#' @include class-CoreSet.R
NULL

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

#' @rdname CoreSet-accessors
#' @details - annotation: getter method for annotation slot.
#'
#' @include class-CoreSet.R
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
#' - annotation<-: setter method for the annotation slot.
#'   - value: a `list` of annotations to update the `CoreSet` with.
#'
#'
#' @include class-CoreSet.R
#'
#' @aliases annotation<-
#'
#' @md
#' @importMethodsFrom BiocGenerics annotation<-
#' @exportMethod `annotation<-`
setReplaceMethod("annotation", signature("CoreSet", "list"), function(object, value) {
    object@annotation <- value
    object
})