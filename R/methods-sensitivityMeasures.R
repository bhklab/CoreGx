#' sensitivityMeasures CoreSet Getter Method
#'
#' @describeIn CoreSet Returns the available sensitivity profile
#'   summaries, for example, whether there are IC50 values available
#'
#' @examples
#' sensitivityMeasures(clevelandSmall_cSet)
#'
#' @param object A [`CoreSet`] object to retrieve the names of sensitivty
#'    profile summary measurements for.
#'
#' @return A [`character`] vector of all the available sensitivity measures.

#' @export
setMethod(sensitivityMeasures, "CoreSet", function(object){
    return(colnames(sensitivityProfiles(object)))
})

#' sensitivityMeasures<- CoreSet Setter Method
#'
#' @describeIn CoreSet Updates the sensitivity measures in a `CoreSet` object
#'     and returns the updated object
#'
#' @examples
#' data(clevelandSmall_cSet)
#' sensitivityMeasures(clevelandSmall_cSet) <- sensitivityMeasures(clevelandSmall_cSet)
#'
#' @param object  The \code{CoreSet} object to update
#' @param value A \code{character} vector of sensitivity measures to use to update the cSet object
#'
#' @return A update \code{CoreSet} object with the new sensitivity measures
#'
#' @export
setReplaceMethod('sensitivityMeasures',
    signature(object='CoreSet', value='character'),
    function(object, value) {
  colnames(sensitivityProfiles(object)) <- value
  object
})