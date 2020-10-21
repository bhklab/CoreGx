#' mDataNames Generic
#' 
#' A generic for the mDataNames method
#'
#' @examples
#' mDataNames(clevelandSmall_cSet)
#' 
#' @param object CoreSet object
#' @param ... Fallthrough arguements for defining new methods
#'  
#' @return Vector of names of the molecular data types
#' 
#' @export
setGeneric("mDataNames", function(object, ...) standardGeneric("mDataNames"))
#'
#' @describeIn CoreSet Return the molecular data types available in a cSet object
#' @export
setMethod("mDataNames", "CoreSet", function(object){
  return(names(object@molecularProfiles))
})

#' mDataNames<- Generic
#'
#' A generic for the mDataNames method
#'
#' @examples
#' mDataNames(clevelandSmall_cSet) <- mDataNames(clevelandSmall_cSet)
#'
#' @param object CoreSet object
#' @param ... Fallthrough arguements for defining new methods
#' @param value A \code{character} vector with names to be assigned to each list item in the `molecularProfiles` slot
#'
#' @return An updated copy of the CoreSet object
#'
#' @export
setGeneric("mDataNames<-", function(object, ..., value) standardGeneric("mDataNames<-"))
#'
#' @describeIn CoreSet Return the molecular data types available in a cSet object
#' @export
setReplaceMethod("mDataNames", "CoreSet", function(object, value){
  names(object@molecularProfiles) <- value
  return(object)
})