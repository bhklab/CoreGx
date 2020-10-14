#' sensitivityInfo Generic
#'
#' Generic for sensitivityInfo method
#'
#' @examples
#' data(clevelandSmall_cSet)
#' sensitivityInfo(clevelandSmall_cSet)
#'
#' @param object The \code{CoreSet} to retrieve sensitivity experiment annotations from
#' @param ... Fallthrough arguments for defining new methods
#'
#' @return a \code{data.frame} with the experiment info
setGeneric("sensitivityInfo", function(object, ...) standardGeneric("sensitivityInfo"))
#' @describeIn CoreSet Return the drug dose sensitivity experiment info
#' @export
setMethod(sensitivityInfo, "CoreSet", function(object){
    return(object@sensitivity$info)
})

#' sensitivityInfo<- Generic
#'
#' A generic for the sensitivityInfo replacement method
#'
#' @examples
#' sensitivityInfo(clevelandSmall_cSet) <- sensitivityInfo(clevelandSmall_cSet)
#'
#' @param object The \code{CoreSet} to update
#' @param ... Fallthrough arguments for defining new methods
#' @param value A \code{data.frame} with the new sensitivity annotations
#'
#' @return Updated \code{CoreSet}
setGeneric("sensitivityInfo<-", function(object, ..., value) standardGeneric("sensitivityInfo<-"))
#' @describeIn CoreSet Update the sensitivity experiment info
#' @export
setReplaceMethod("sensitivityInfo",
                 signature = signature(object="CoreSet",
                                       value="data.frame"),
                 function(object, value){
    object@sensitivity$info <- value
    object
})