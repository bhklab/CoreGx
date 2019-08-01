#' phenotype Generic
#' 
#' Generic for phenotype method 
#' 
#' @examples
#' data(CCLEsmall)
#' phenotype(CCLEsmall)
#' 
#' @param x The \code{PhenoGenoExperiment} to retrieve phenotype experiment annotations from
#' @return a \code{data.frame} with the experiment info
setGeneric("phenotype", function(x) standardGeneric("phenotype"))
#' @describeIn PhenoGenoExperiment Return the drug dose phenotype experiment info
#' @export
setMethod(phenotype, "PhenoGenoExperiment", function(x){
    return(x@phenotype)
})


#' phenotype<- Generic
#' 
#' A generic for the phenotype replacement method
#' 
#' 
#' @examples
#' data(CCLEsmall)
#' phenotype(CCLEsmall) <- phenotype(CCLEsmall)
#' 
#' @param object The \code{PhenoGenoExperiment} to update
#' @param value A \code{data.frame} with the new phenotype annotations
#' @return Updated \code{PhenoGenoExperiment} 
setGeneric("phenotype<-", function(object, value) standardGeneric("phenotype<-"))
#' @describeIn PhenoGenoExperiment Update the phenotype experiment info
#' @export
setReplaceMethod("phenotype", signature = signature(object="PhenoGenoExperiment",value="ANY"), function(object, value){
    BiocGenerics:::replaceSlots(object, phenotype = value)
    object
})





#' phenotypeInfo Generic
#' 
#' Generic for phenotypeInfo method 
#' 
#' @examples
#' data(CCLEsmall)
#' phenotypeInfo(CCLEsmall)
#' 
#' @param x The \code{PhenoGenoExperiment} to retrieve phenotype experiment annotations from
#' @return a \code{data.frame} with the experiment info
setGeneric("phenotypeInfo", function(x) standardGeneric("phenotypeInfo"))
#' @describeIn PhenoGenoExperiment Return the drug dose phenotype experiment info
#' @export
setMethod(phenotypeInfo, "PhenoGenoExperiment", function(x){
    return(x@phenotype$info)
})

#' phenotypeInfo<- Generic
#' 
#' A generic for the phenotypeInfo replacement method
#' 
#' 
#' @examples
#' data(CCLEsmall)
#' phenotypeInfo(CCLEsmall) <- phenotypeInfo(CCLEsmall)
#' 
#' @param object The \code{PhenoGenoExperiment} to update
#' @param value A \code{data.frame} with the new phenotype annotations
#' @return Updated \code{PhenoGenoExperiment} 
setGeneric("phenotypeInfo<-", function(object, value) standardGeneric("phenotypeInfo<-"))
#' @describeIn PhenoGenoExperiment Update the phenotype experiment info
#' @export
setReplaceMethod("phenotypeInfo", signature = signature(object="PhenoGenoExperiment",value="ANY"), function(object, value){
    object@phenotype$info <- value
    object
})


#' phenotypeProfiles Generic
#' 
#' Generic for phenotypeProfiles method 
#' 
#' @examples
#' data(CCLEsmall)
#' phenotypeProfiles(CCLEsmall)
#' 
#' @param x The \code{PhenoGenoExperiment} to retrieve phenotype experiment data from
#' @return a \code{data.frame} with the experiment info
setGeneric("phenotypeProfiles", function(x) standardGeneric("phenotypeProfiles"))
#' @describeIn PhenoGenoExperiment Return the phenotypic data for the drug dose phenotype
#' @export
setMethod(phenotypeProfiles, "PhenoGenoExperiment", function(x){
    return(x@phenotype$profiles)
})

#' phenotypeProfiles<- Generic
#' 
#' A generic for the phenotypeProfiles replacement method
#' 
#' @examples
#' data(CCLEsmall)
#' phenotypeProfiles(CCLEsmall) <- phenotypeProfiles(CCLEsmall)
#' 
#' @param object The \code{PhenoGenoExperiment} to update
#' @param value A \code{data.frame} with the new phenotype profiles. If a matrix object is passed in, converted to data.frame before assignment
#' @return Updated \code{PhenoGenoExperiment} 
setGeneric("phenotypeProfiles<-", function(object, value) standardGeneric("phenotypeProfiles<-"))
# #' @describeIn PhenoGenoExperiment Update the phenotypic data for the drug dose
# #'   phenotype
# #' @export
# setReplaceMethod("phenotypeProfiles", signature = signature(object="PhenoGenoExperiment",value="ANY"), function(object, value){
#     object@phenotype$profiles <- value
#     object
# })
#' @describeIn PhenoGenoExperiment Update the phenotypic data for the drug dose
#'   phenotype
#' @export
setReplaceMethod("phenotypeProfiles", signature = signature(object="PhenoGenoExperiment",value="ANY"), function(object, value){
    object@phenotype$profiles <- as.data.frame(value)
    object
})
#' phenotypeMeasures Generic
#' 
#' A generic for the phenotypeMeasures  method
#' 
#' @examples
#' data(CCLEsmall)
#' phenotypeMeasures(CCLEsmall)
#' 
#' @param x The \code{PhenoGenoExperiment} 
#' @return A \code{character} vector of all the available phenotype measures
setGeneric("phenotypeMeasures", function(x) standardGeneric("phenotypeMeasures"))
#' @describeIn PhenoGenoExperiment Returns the available phenotype profile
#'   summaries, for example, whether there are IC50 values available
#' @export
setMethod(phenotypeMeasures, "PhenoGenoExperiment", function(x){
    return(setdiff(colnames(phenotypeProfiles(x)), getIDColNames(x@phenotype)))
    
})