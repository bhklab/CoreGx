#' annotation Slot Getter
#'
#' @param object A \code{RadioSet}
#'
#' @return A \code{list} of named annotaiton
#'
#' @examples
#' annotation(clevelandSmall_cSet)
#'
#' @describeIn CoreSet Retrieve the annotations slot form an rSet
#'
#' @importMethodsFrom BiocGenerics annotation
#' @export
setMethod('annotation', signature("CoreSet"), function(object) {
    object@annotation
})

#' annotation<- Slot Setter
#'
#' @param object A \code{RadioSet}
#' @param value A \code{list} of annotations to add to the annotatiosn slot of
#'   an rSet
#'
#' @return A copy of the \code{CoreSet} with the updated annotation slot
#'
#' @examples
#' annotation(clevelandSmall_cSet) <- annotation(clevelandSmall_cSet)
#'
#' @describeIn RadioSet Update the annotation slot of a tSet
#'
#' @importMethodsFrom BiocGenerics annotation<-
#' @export
setReplaceMethod("annotation", signature("CoreSet", "list"), function(object, value) {
    object@annotation <- value
    object
})
