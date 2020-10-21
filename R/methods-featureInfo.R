#' featureInfo Generic
#'
#' Generic for featureInfo method
#'
#' @examples
#' featureInfo(clevelandSmall_cSet, "rna")
#'
#' @param object The \code{CoreSet} to retrieve feature annotations from
#' @param mDataType the type of molecular data
#' @param ... Fallthrough arguements for defining new methods
#'
#' @return a \code{data.frame} with the feature annotations
#'
setGeneric("featureInfo", function(object, mDataType, ...) standardGeneric("featureInfo"))
#'
#' @describeIn CoreSet Return the feature info for the given molecular data
#' @importFrom SummarizedExperiment rowData rowData<-
#' @export
setMethod(featureInfo, "CoreSet", function(object, mDataType) {
  if(mDataType %in% names(object@molecularProfiles)){
    return(rowData(object@molecularProfiles[[mDataType]]))
  }else{
    return(NULL)
  }
})

#' featureInfo<- Generic
#'
#' Generic for featureInfo replace method
#'
#' @examples
#' featureInfo(clevelandSmall_cSet, "rna") <- featureInfo(clevelandSmall_cSet, "rna")
#'
#' @param object The \code{CoreSet} to replace gene annotations in
#' @param mDataType The type of molecular data to be updated
#' @param value A \code{data.frame} with the new feature annotations
#'
#' @return Updated \code{CoreSet}
#'
#' @export
setGeneric("featureInfo<-", function(object, mDataType, value) standardGeneric("featureInfo<-"))
#' @describeIn CoreSet Replace the gene info for the molecular data
#' @importFrom SummarizedExperiment rowData rowData<-
#' @importFrom S4Vectors DataFrame
#'
#' @export
setReplaceMethod("featureInfo", signature = signature(object="CoreSet", mDataType ="character",value="data.frame"), function(object, mDataType, value){

  if(mDataType %in% names(object@molecularProfiles)){
    rowData(object@molecularProfiles[[mDataType]]) <-
      S4Vectors::DataFrame(value, rownames = rownames(value))
  }
  object
})
#' @describeIn CoreSet Replace the gene info for the molecular data
#'@export
setReplaceMethod("featureInfo", signature = signature(object="CoreSet", mDataType ="character",value="DataFrame"), function(object, mDataType, value){

  if(mDataType %in% names(object@molecularProfiles)){
    rowData(object@molecularProfiles[[mDataType]]) <-
      S4Vectors::DataFrame(value, rownames = rownames(value))
  }
  object
})

