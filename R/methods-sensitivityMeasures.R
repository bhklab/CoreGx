#' sensitivityMeasures Generic
#'
#' A generic for the sensitivityMeasures  method
#'
#' @examples
#' sensitivityMeasures(clevelandSmall_cSet)
#'
#' @param object The \code{CoreSet}
#' @param ... Fallthrough arguements for defining new methods
#'
#' @return A \code{character} vector of all the available sensitivity measures
setGeneric("sensitivityMeasures", function(object, ...) standardGeneric("sensitivityMeasures"))
#' @describeIn CoreSet Returns the available sensitivity profile
#'   summaries, for example, whether there are IC50 values available
#' @export
setMethod(sensitivityMeasures, "CoreSet", function(object){
    return(colnames(sensitivityProfiles(object)))
})

#' sensitivityMeasures<- Generic
#'
#' A generic for the sensitivityMeasure<- method
#'
#' @examples
#' sensitivityMeasures(clevelandSmall_cSet) <- sensitivityMeasures(clevelandSmall_cSet)
#'
#' @param object  The \code{CoreSet} object to update
#' @param ... Fallthrough arguements for defining new methods
#' @param value A \code{character} vector of sensitivity measures to use to update the cSet object
#'
#' @return A update \code{CoreSet} object with the new sensitivity measures
setGeneric('sensitivityMeasures<-', function(object, ..., value) standardGeneric('sensitivityMeasures<-'))
#' @describeIn CoreSet Updates the sensitivity measures in a cSet object and returns the updated object
#' @export
setReplaceMethod('sensitivityMeasures', signature(object='CoreSet', value='character'), function(object, value) {
  colnames(sensitivityProfiles(object)) <- value
})