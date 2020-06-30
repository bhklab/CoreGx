#' cellInfo Getter
#'
#' Get cell line information from a PharmacoSet object
#'
#' @examples
#' data(clevelandSmall_cSet)
#' cellInf <- cellInfo(clevelandSmall_cSet)
#'
#' @param object The \code{CoreSet} to retrieve cell info from
#' @param ... \code{list} Fall through arguments to allow generic
#'   to be defined with different parameters
#'
#' @return a \code{data.frame} with the cell annotations
#'
#' @import methods
#'
#' @export
setGeneric("cellInfo", function(object, ...) standardGeneric("cellInfo"))
#' @describeIn CoreSet Returns the annotations for all the cell lines tested on in the CoreSet
#' @export
setMethod(cellInfo, "CoreSet", function(object){
  object@cell
})


#' cellInfo<- Generic
#'
#' Generic for cellInfo replace method
#'
#' @examples
#' cellInfo(clevelandSmall_cSet) <- cellInfo(clevelandSmall_cSet)
#'
#' @param object The \code{CoreSet} to replace cell info in
#' @param value A \code{data.frame} with the new cell annotations
#'
#' @return Updated \code{CoreSet}
#'
#' @export
setGeneric("cellInfo<-", function(object, value) standardGeneric("cellInfo<-"))
#' @describeIn CoreSet Update the cell line annotations
#' @export
setReplaceMethod("cellInfo", signature = signature(object="CoreSet", value="data.frame"), function(object, value){
  if(is.null(rownames(value))){
    stop("Please provide the cell_id as rownames for the cell line annotations")
  }
  object@cell <- value
  object
})