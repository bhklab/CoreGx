#' sensitivityInfo Generic
#'
#' Generic for sensitivityInfo method
#'
#' @examples
#' data(clevelandSmall_cSet)
#' sensitivityInfo(clevelandSmall_cSet)
#'
#' @param object The [`CoreSet`] to retrieve sensitivity experiment annotations from
#'
#' @return a \code{data.frame} with the experiment info if dimension is excluded,
#'   otherwise a `data.table` with annotations for the cells or drugs dimension
#'   of the LongTable.
#'
#' @describeIn CoreSet Return the drug dose sensitivity experiment info
#' @export
setMethod(sensitivityInfo, "CoreSet", function(object) {

    if (is(sensitivitySlot(object), 'LongTable'))
        stop(.errorMsg('\n[CoreGx::sensitivityInfo] No getter is implemented ',
            'for @sensitviity containing a LongTable in a CoreSet. Please define',
            ' a new method using setMethod() in the current package!'))

    return(object@sensitivity$info)
})

#' sensitivityInfo<- Generic
#'
#' A generic for the sensitivityInfo replacement method
#'
#' @examples
#' sensitivityInfo(clevelandSmall_cSet) <- sensitivityInfo(clevelandSmall_cSet)
#'
#' @param object The \code{PharmacoSet} to update
#' @param value A \code{DataFrame} with the new sensitivity annotations
#'
#' @return Updated \code{CoreSet}
#'
#' @describeIn CoreSet Update the sensitivity experiment info
#' @export
setReplaceMethod("sensitivityInfo",
                 signature(object="CoreSet", value="data.frame"),
                 function(object, value) {
    if (is(sensitivitySlot(object), 'LongTable'))
        stop(.errorMsg('\n[CoreGx::sensitivityInfo<-] No setter is implemented ',
            'for @sensitviity containing a LongTable in a CoreSet. Please define',
            ' a new method using setMethod() in the current package!'))

    object@sensitivity$info <- value
    return(object)
})