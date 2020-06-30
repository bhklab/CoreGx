#' name Generic
#'
#' A generic for the name method
#'
#' @examples
#' name(clevelandSmall_cSet)
#'
#' @param object A \code{CoreSet}
#' @param ... Fallthrough arguements for defining new methods
#'
#' @return The name of the CoreSet
#'
setGeneric("name", function(object, ...) standardGeneric("name"))
#' @describeIn CoreSet Return the name of the CoreSet
#' @export
setMethod('name', signature("CoreSet"), function(object){
    return(object@annotation$name)
})

#' name<- Generic
#'
#' A generic for the name<- method
#'
#' @examples
#' name(clevelandSmall_cSet) <- "Cleveland Small"
#'
#' @param object A \code{CoreSet} object
#' @param ... Fallthrough arguements for defining new methods
#' @param value A \code{character} string with the name to assign to the cSet
#'
#' @return The name of the CoreSet
#'
setGeneric("name<-", function(object, ..., value) standardGeneric("name<-"))
#' @describeIn CoreSet Return the name of the CoreSet
#' @export
setReplaceMethod('name', signature("CoreSet"), function(object, value){
    object@annotation$name <- value
    return(object)
})