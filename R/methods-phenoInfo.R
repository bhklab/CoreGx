#' phenoInfo Generic
#'
#' Generic for phenoInfo method
#'
#' @examples
#' phenoInfo(clevelandSmall_cSet, mDataType="rna")
#'
#' @param object The \code{CoreSet} to retrieve rna annotations from
#' @param mDataType the type of molecular data
#' @param ... Fallthrough argument for defining new parameters in other S4 methods
#'
#' @return a \code{data.frame} with the experiment info
#'
setGeneric("phenoInfo", function(object, mDataType, ...) standardGeneric("phenoInfo"))
#'
#' @importFrom SummarizedExperiment colData
#' @describeIn CoreSet Return the experiment info from the given type of molecular data in CoreSet
#' @export
setMethod(phenoInfo, "CoreSet", function(object, mDataType){

  if(mDataType %in% names(object@molecularProfiles)){ # Columns = Samples
    return(colData(object@molecularProfiles[[mDataType]]))
  }else{
    return(NULL)
  }
})

#' phenoInfo<- Generic
#'
#' Generic for phenoInfo replace method
#'
#' @examples
#' data(clevelandSmall_cSet)
#' phenoInfo(clevelandSmall_cSet, mDataType="rna") <- phenoInfo(clevelandSmall_cSet, mDataType="rna")
#'
#' @param object The \code{CoreSet} to retrieve molecular experiment annotations from
#' @param mDataType the type of molecular data
#' @param value a \code{dataframe}  with the new experiment annotations
#'
#' @return The updated \code{CoreSet}
#'
setGeneric("phenoInfo<-", function(object, mDataType, value) standardGeneric("phenoInfo<-"))
#' @describeIn CoreSet Update the given type of molecular data experiment info in the CoreSet
#' @importFrom SummarizedExperiment colData colData<-
#' @importFrom S4Vectors DataFrame
#' @export
setReplaceMethod("phenoInfo", signature = signature(object="CoreSet", mDataType ="character", value="data.frame"), function(object, mDataType, value){
  if(mDataType %in% names(object@molecularProfiles)){
    SummarizedExperiment::colData(object@molecularProfiles[[mDataType]]) <- S4Vectors::DataFrame(value, rownames = rownames(value))
  }
  object
})
#' @describeIn CoreSet Update the given type of molecular data experiment info in the CoreSet
#' @export
setReplaceMethod("phenoInfo", signature = signature(object="CoreSet", mDataType ="character", value="DataFrame"), function(object, mDataType, value){
  if(mDataType %in% names(object@molecularProfiles)){
    SummarizedExperiment::colData(object@molecularProfiles[[mDataType]]) <- value
  }
  object
})

