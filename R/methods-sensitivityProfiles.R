#' sensitivityProfiles CoreSet Method
#'
#' @describeIn CoreSet Return the sensitivity profile summaries from the
#'    sensitivity slot.
#'
#' @examples
#' sensitivityProfiles(clevelandSmall_cSet)
#'
#' @param object The [`CoreSet`] to retrieve sensitivity profile summaries
#'   from.
#'
#' @return a [`data.frame`] with sensitivity profile summaries for CoreSet
#'
#' @export
setMethod(sensitivityProfiles, "CoreSet", function(object) {
    funContext <- paste0()
    if (is(sensitivitySlot(object), 'LongTable')) {
        if (!('profiles' %in% assayNames(sensitivitySlot(object)))) 
            stop(.errorMsg())
    } else {
        return(object@sensitivity$profiles)
    }
})

#' sensitivityProfiles<- CoreSet Method
#'
#' @describeIn CoreSet Update the sensitivity profile summaries the sensitivity
#'    slot.
#'
#' @examples
#' sensitivityProfiles(clevelandSmall_cSet) <- sensitivityProfiles(clevelandSmall_cSet)
#'
#' @param object The \code{CoreSet} to update
#' @param value A \code{data.frame} with the new sensitivity profiles. If a
#'   matrix object is passed in, converted to data.frame before assignment
#'
#' @return Updated \code{CoreSet}
#'
#' @export
setReplaceMethod("sensitivityProfiles",
                 signature = signature(object="CoreSet",
                                       value="data.frame"),
                 function(object, value){
    if (is(sensitivitySlot(object), 'LongTable'))
        stop(.errorMsg('[CoreGx::sensitivityProfiles<-] No setter has been ',
            'implemented for this method when the sensitivity slot in a CoreSet',
            ' is a LongTable. Please define a method using setReplaceMethod() ',
            'for the subclass of CoreSet in your current package!'))
    else
        object@sensitivity$profiles <- value
    return(object)
})
#' @describeIn CoreSet Update the phenotypic data for the drug dose
#'   sensitivity
#'
#' @export
setReplaceMethod("sensitivityProfiles",
                 signature = signature(object="CoreSet",
                                       value="matrix"),
                function(object, value) {
    if (is(sensitivitySlot(object), 'LongTable'))
        stop(.errorMsg('[CoreGx::sensitivityProfiles<-] No setter has been ',
            'implemented for this method when the sensitivity slot in a CoreSet',
            ' is a LongTable. Please define a method using setReplaceMethod() ',
            'for the subclass of CoreSet in your current package!'))
    else
        object@sensitivity$profiles <- as.data.frame(value)
    return(object)
})
