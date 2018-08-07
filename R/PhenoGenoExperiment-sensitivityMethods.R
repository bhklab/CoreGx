
#' sensitivityInfo Generic
#' 
#' Generic for sensitivityInfo method 
#' 
#' @examples
#' data(CCLEsmall)
#' sensitivityInfo(CCLEsmall)
#' 
#' @param x The \code{PhenoGenoExperiment} to retrieve sensitivity experiment annotations from
#' @return a \code{data.frame} with the experiment info
setGeneric("sensitivityInfo", function(x) standardGeneric("sensitivityInfo"))
#' @describeIn PhenoGenoExperiment Return the drug dose sensitivity experiment info
#' @export
setMethod(sensitivityInfo, "PhenoGenoExperiment", function(x){
    
    return(x@sensitivity$info)
    
})

#' sensitivityInfo<- Generic
#' 
#' A generic for the sensitivityInfo replacement method
#' 
#' 
#' @examples
#' data(CCLEsmall)
#' sensitivityInfo(CCLEsmall) <- sensitivityInfo(CCLEsmall)
#' 
#' @param object The \code{PhenoGenoExperiment} to update
#' @param value A \code{data.frame} with the new sensitivity annotations
#' @return Updated \code{PhenoGenoExperiment} 
setGeneric("sensitivityInfo<-", function(object, value) standardGeneric("sensitivityInfo<-"))
#' @describeIn PhenoGenoExperiment Update the sensitivity experiment info
#' @export
setReplaceMethod("sensitivityInfo", signature = signature(object="PhenoGenoExperiment",value="data.frame"), function(object, value){

    object@sensitivity$info <- value
    object
})


#' sensitivityProfiles Generic
#' 
#' Generic for sensitivityProfiles method 
#' 
#' @examples
#' data(CCLEsmall)
#' sensitivityProfiles(CCLEsmall)
#' 
#' @param x The \code{PhenoGenoExperiment} to retrieve sensitivity experiment data from
#' @return a \code{data.frame} with the experiment info
setGeneric("sensitivityProfiles", function(x) standardGeneric("sensitivityProfiles"))
#' @describeIn PhenoGenoExperiment Return the phenotypic data for the drug dose sensitivity
#' @export
setMethod(sensitivityProfiles, "PhenoGenoExperiment", function(x){
    
    return(x@sensitivity$profiles)
    
})

#' sensitivityProfiles<- Generic
#' 
#' A generic for the sensitivityProfiles replacement method
#' 
#' @examples
#' data(CCLEsmall)
#' sensitivityProfiles(CCLEsmall) <- sensitivityProfiles(CCLEsmall)
#' 
#' @param object The \code{PhenoGenoExperiment} to update
#' @param value A \code{data.frame} with the new sensitivity profiles. If a matrix object is passed in, converted to data.frame before assignment
#' @return Updated \code{PhenoGenoExperiment} 
setGeneric("sensitivityProfiles<-", function(object, value) standardGeneric("sensitivityProfiles<-"))
#' @describeIn PhenoGenoExperiment Update the phenotypic data for the drug dose
#'   sensitivity
#' @export
setReplaceMethod("sensitivityProfiles", signature = signature(object="PhenoGenoExperiment",value="data.frame"), function(object, value){

    object@sensitivity$profiles <- value
    object
})
#' @describeIn PhenoGenoExperiment Update the phenotypic data for the drug dose
#'   sensitivity
#' @export
setReplaceMethod("sensitivityProfiles", signature = signature(object="PhenoGenoExperiment",value="matrix"), function(object, value){

    object@sensitivity$profiles <- as.data.frame(value)
    object
})
#' sensitivityMeasures Generic
#' 
#' A generic for the sensitivityMeasures  method
#' 
#' @examples
#' data(CCLEsmall)
#' sensitivityMeasures(CCLEsmall)
#' 
#' @param x The \code{PhenoGenoExperiment} 
#' @return A \code{character} vector of all the available sensitivity measures
setGeneric("sensitivityMeasures", function(x) standardGeneric("sensitivityMeasures"))
#' @describeIn PhenoGenoExperiment Returns the available sensitivity profile
#'   summaries, for example, whether there are IC50 values available
#' @export
setMethod(sensitivityMeasures, "PhenoGenoExperiment", function(x){
    
    return(colnames(sensitivityProfiles(x)))
    
})