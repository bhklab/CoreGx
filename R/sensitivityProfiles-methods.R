#' sensitivityProfiles Generic
#'
#' Generic for sensitivityProfiles method
#'
#' @examples
#' sensitivityProfiles(clevelandSmall_cSet)
#'
#' @param object The \code{CoreSet} to retrieve sensitivity experiment data from
#' @param ... Fallthrough arguements for defining new methods
#'
#' @return a \code{data.frame} with the experiment info
#' @export
setGeneric("sensitivityProfiles", function(object, ...) standardGeneric("sensitivityProfiles"))
#' @describeIn CoreSet Return the phenotypic data for the drug dose sensitivity
#' @export
setMethod(sensitivityProfiles, "CoreSet", function(object){
    return(object@sensitivity$profiles)
})

#' sensitivityProfiles<- Generic
#'
#' A generic for the sensitivityProfiles replacement method
#'
#' @examples
#' sensitivityProfiles(clevelandSmall_cSet) <- sensitivityProfiles(clevelandSmall_cSet)
#'
#' @param object The \code{CoreSet} to update
#' @param ... Fallthrough arguments for defining new methods
#' @param value A \code{data.frame} with the new sensitivity profiles. If a matrix object is passed in, converted to data.frame before assignment
#'
#' @return Updated \code{CoreSet}
#'
#' @export
setGeneric("sensitivityProfiles<-", function(object, ..., value) standardGeneric("sensitivityProfiles<-"))
#' @describeIn CoreSet Update the phenotypic data for the drug dose
#'   sensitivity
#' @export
setReplaceMethod("sensitivityProfiles",
                 signature = signature(object="CoreSet",
                                       value="data.frame"),
                 function(object, value){
    object@sensitivity$profiles <- value
    object
})
#' @describeIn CoreSet Update the phenotypic data for the drug dose
#'   sensitivity
#' @export
setReplaceMethod("sensitivityProfiles",
                 signature = signature(object="CoreSet",
                                       value="matrix"),
                 function(object, value) {
    object@sensitivity$profiles <- as.data.frame(value)
    object
})
