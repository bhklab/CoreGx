#' fNames Generic
#'
#' A generic for the fNames method
#'
#' @examples
#' fNames(clevelandSmall_cSet, "rna")
#'
#' @param object The \code{CoreSet}
#' @param mDataType The molecular data type to return feature names for
#' @param ... Fallthrough arguements for defining new methods
#'
#' @return A \code{character} vector of the feature names
setGeneric("fNames", function(object, mDataType, ...) standardGeneric("fNames"))
#' @describeIn CoreSet Return the feature names used in the dataset
#' @export
setMethod(fNames, "CoreSet", function(object, mDataType){
  if (mDataType %in% names(object@molecularProfiles)) {
    rownames(featureInfo(object, mDataType))
  } else {
    stop(paste0("Molecular data type ", mDataType ," is not part of this ", class(object)))
  }
})

#' fNames<- Generic
#'
#' A generic for the fNames replacement method
#'
#' @examples
#' data(clevelandSmall_cSet)
#' fNames(clevelandSmall_cSet, "rna") <- fNames(clevelandSmall_cSet, "rna")
#'
#' @param object The \code{CoreSet} to update
#' @param mDataType The molecular data type to update
#' @param value A \code{character} vector of the new cell names
#'
#' @return Updated \code{CoreSet}
#' @export
setGeneric("fNames<-", function(object, mDataType, value)
  standardGeneric("fNames<-"))
#' @describeIn CoreSet Update the feature names used in a molecular profile
#' @export
setReplaceMethod("fNames",
                 signature = signature(object="CoreSet", mDataType='character',
                                       value="character"),
                 function(object, mDataType, value)
{
  if (mDataType %in% names(object@molecularProfiles)) {
    rownames(featureInfo(object, mDataType)) <- as.character(value)
    object
  } else {
    stop(paste0(mDataType, " is not in the object. Possible mDataTypes are: ", names(object@molecularProfiles)))
  }
})