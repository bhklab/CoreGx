#' pertNumber Generic
#' 
#' A generic for the pertNumber method
#' 
#' @examples
#' pertNumber(clevelandSmall_cSet)
#' 
#' @param object A \code{CoreSet}
#' @param ... Fallthrough arguements for defining new methods
#'
#' @return A 3D \code{array} with the number of perturbation experiments per drug and cell line, and data type
#' @export
setGeneric("pertNumber", function(object, ...) standardGeneric("pertNumber"))
#' 
#' @describeIn CoreSet Return the summary of available perturbation
#'   experiments
#' @export
setMethod(pertNumber, "CoreSet", function(object){
    return(object@perturbation$n)
})

#' pertNumber<- Generic
#'
#' A generic for the pertNumber method
#'
#' @examples
#' pertNumber(clevelandSmall_cSet) <- pertNumber(clevelandSmall_cSet)
#'
#' @param object A \code{CoreSet}
#' @param value A new 3D \code{array} with the number of perturbation experiments per drug and cell line, and data type
#'
#' @return The updated \code{CoreSet}
#'
#' @export
setGeneric("pertNumber<-", function(object, value) standardGeneric("pertNumber<-"))
#' @describeIn CoreSet Update the summary of available perturbation
#'   experiments
#' @export
setReplaceMethod('pertNumber', signature = signature(object="CoreSet", value="array"), function(object, value){
  object@perturbation$n <- value
  object
})