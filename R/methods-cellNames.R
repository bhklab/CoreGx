#' cellNames Generic
#'
#' A generic for the cellNames method
#'
#' @examples
#' cellNames(clevelandSmall_cSet)
#'
#' @param object The \code{CoreSet} to return cell names from
#' @param ... Fallthrough arguements for defining new methods
#'
#' @return A vector of the cell names used in the CoreSet
setGeneric("cellNames", function(object, ...) standardGeneric("cellNames"))
#' @describeIn CoreSet Return the cell names used in the dataset
#' @export
setMethod(cellNames, signature("CoreSet"), function(object){
  rownames(cellInfo(object))
})

#' cellNames<- Generic
#'
#' A generic for the cellNames replacement method
#'
#' @examples
#' cellNames(clevelandSmall_cSet) <- cellNames(clevelandSmall_cSet)
#'
#' @param object The \code{CoreSet} to update
#' @param ... Fallthrough arguments for defining new methods
#' @param value A \code{character} vector of the new cell names
#'
#' @return Updated \code{CoreSet}
#' @export
setGeneric("cellNames<-", function(object, ..., value) standardGeneric("cellNames<-"))
#' @describeIn CoreSet Update the cell names used in the dataset
#' @export
setReplaceMethod("cellNames", signature(object="CoreSet",value="character"), function(object, value){
    object <- updateCellId(object, value)
    return(object)
})
