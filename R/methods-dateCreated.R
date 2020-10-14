#' dateCreated Generic
#'
#' A generic for the dateCreated method
#'
#' @examples
#' dateCreated(clevelandSmall_cSet)
#'
#' @param object A \code{CoreSet}
#' @param ... Fallthrough arguements for defining new methods
#'
#' @return The date the CoreSet was created
setGeneric("dateCreated", function(object, ...) standardGeneric("dateCreated"))
#' @describeIn CoreSet Return the date the CoreSet was created
#' @export
setMethod('dateCreated', signature("CoreSet"), function(object) {
  object@annotation$dateCreated
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
setGeneric("dateCreated<-", function(object, ..., value) standardGeneric("dateCreated<-"))
#' @describeIn CoreSet Update the date a cSet was created
#' @export
setReplaceMethod('dateCreated', signature("CoreSet"), function(object, value) {
  object@annotation$dateCreated <- value
  return(object)
})


