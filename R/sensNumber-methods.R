#' sensNumber Generic
#'
#' A generic for the sensNumber method
#'
#' @examples
#' sensNumber(clevelandSmall_cSet)
#'
#' @param object A \code{CoreSet}
#' @param ... Fallthrough arguements for defining new methods
#'
#' @return A \code{data.frame} with the number of sensitivity experiments per drug and cell line
#' @export
setGeneric("sensNumber", function(object, ...) standardGeneric("sensNumber"))
#' @describeIn CoreSet Return the summary of available sensitivity
#'   experiments
#' @export
setMethod(sensNumber, "CoreSet", function(object){
  return(object@sensitivity$n)
})

#' sensNumber<- Generic
#'
#' A generic for the sensNumber method
#'
#' @examples
#' sensNumber(clevelandSmall_cSet) <- sensNumber(clevelandSmall_cSet)
#'
#' @param object A \code{CoreSet}
#' @param value A new \code{data.frame} with the number of sensitivity experiments per drug and cell line
#'
#' @return The updated \code{CoreSet}
#'
#' @export
setGeneric("sensNumber<-", function(object, value) standardGeneric("sensNumber<-"))
#' @describeIn CoreSet Update the summary of available sensitivity
#'   experiments
#' @export
setReplaceMethod('sensNumber', signature = signature(object="CoreSet", value="matrix"), function(object, value){
  object@sensitivity$n <- value
  object
})