#' molecularProfiles Generic
#'
#' Generic for molecularProfiles method
#'
#' @examples
#' data(clevelandSmall_cSet)
#' molecularProfiles(clevelandSmall_cSet, "rna")
#'
#' @param object The \code{CoreSet} to retrieve molecular profiles from
#' @param mDataType \code{character} The type of molecular data
#' @param assay \code{character} Name of the desired assay; if excluded
#'   defaults to first assay in the SummarizedExperiment for the given
#'   mDataType. Use \code{assayNames(molecularProfiles(object, dataType)}
#'   to check which assays are available for a given molecular datatype.
#' @param ... Fallthrough arguements for defining new methods
#'
#' @return a \code{matrix} of data for the given mDataType and assay
#'
#' @importClassesFrom S4Vectors DataFrame SimpleList
#' @importFrom S4Vectors DataFrame
#' @importFrom SummarizedExperiment colData assay assayNames
setGeneric("molecularProfiles", function(object, mDataType, assay, ...) standardGeneric("molecularProfiles"))
#' @describeIn CoreSet Return the given type of molecular data from the CoreSet
#' @inheritParams molecularProfiles
#' @export
setMethod(molecularProfiles, "CoreSet", function(object, mDataType, assay){
  ## TODO:: Add an all option that returns a list?
  if(mDataType %in% names(object@molecularProfiles)){
    if (!missing(assay)) {
      if (assay %in% assayNames(object@molecularProfiles[[mDataType]])) {
        return(SummarizedExperiment::assay(object@molecularProfiles[[mDataType]], assay))
      } else {
        stop(paste('Assay', assay, 'not found in the SummarizedExperiment object!'))
      }
    } else {
      return(SummarizedExperiment::assay(object@molecularProfiles[[mDataType]], 1))
    }
  } else {
    stop(paste0('mDataType ', mDataType, ' not found the object!'))
  }
})

#' molecularProfiles<- Generic
#'
#' Generic for molecularProfiles replace method
#'
#' @examples
#' data(clevelandSmall_cSet)
#' molecularProfiles(clevelandSmall_cSet, "rna") <- molecularProfiles(clevelandSmall_cSet, "rna")
#'
#' @param object The \code{CoreSet} to replace molecular profiles in
#' @param mDataType The type of molecular data to be updated
#' @param assay \code{character} Name or index of the assay data to return
#' @param value A \code{matrix} with the new profiles
#'
#' @return Updated \code{CoreSet}
#'
setGeneric("molecularProfiles<-", function(object, mDataType, assay, value) standardGeneric("molecularProfiles<-"))
#' @describeIn CoreSet Update the given type of molecular data from the CoreSet
#' @importFrom SummarizedExperiment assay
#' @inheritParams molecularProfiles<-
#' @export
setReplaceMethod("molecularProfiles", signature = signature(object="CoreSet", mDataType ="character", assay="character", value="matrix"), function(object, mDataType, assay, value){
  if (mDataType %in% names(object@molecularProfiles)) {
    SummarizedExperiment::assay(object@molecularProfiles[[mDataType]], assay) <- value
  }
  object
})
#' @describeIn CoreSet Update the given type of molecular data from the CoreSet
#' @inheritParams molecularProfiles<-
#' @export
setReplaceMethod("molecularProfiles",
                 signature(object="CoreSet", mDataType ="character",
                           assay="missing", value="matrix"),
                 function(object, mDataType, assay, value) {
  if (mDataType %in% names(object@molecularProfiles)) {
    SummarizedExperiment::assay(object@molecularProfiles[[mDataType]], 1) <- value
  }
  object
})