#' sensitivityInfo Generic
#'
#' Generic for sensitivityInfo method
#'
#' @examples
#' data(clevelandSmall_cSet)
#' sensitivityInfo(clevelandSmall_cSet)
#'
#' @param object The [`CoreSet`] to retrieve sensitivity experiment annotations from
#' @param dimension [`character`] Optional name of the dimension to extract,
#'     either 'cells' or 'drugs'. Only used if the sensitivity slot contains a
#'     `LongTable` object instead of a `list`.
#' @param ... [`pairlist`] Additional arguments to the rowData or colData
#'     `LongTable` methods. Only used if the sensitivity slot contains a
#'     `LongTable` object instead of a `list`.
#'
#' @return a \code{data.frame} with the experiment info if dimension is excluded,
#'   otherwise a `data.table` with annotations for the cells or drugs dimension
#'   of the LongTable.
#'
#' @describeIn CoreSet Return the drug dose sensitivity experiment info
#' @export
setMethod(sensitivityInfo, "CoreSet", function(object, dimension=c('cells', 'drugs'), ...){

  # case where sensitivity slot is a LongTable
  if (is(sensitivitySlot(object), 'LongTable')) {
    if (!missing(dimension)) {
        dimension <- match.arg(dimension)
        if (dimension == 'cells')
            return(rowData(sensitivitySlot(object), ...))
        else
            return(colData(sensitivitySlot(object), ...))
    } else {
      return(.rebuildInfo(sensitivitySlot(object)))
    }
  # sensitivity is a list
  } else {
    if (!missing(dimension))
      warning(.warnMsg('The dimension argument is only valid if the sensitivity',
        'slot contains a LongTable object. Ignoring function parameters'))
    return(sensitivitySlot(object)$info)
  }

})

#' sensitivityInfo<- Generic
#'
#' A generic for the sensitivityInfo replacement method
#'
#' @examples
#' sensitivityInfo(clevelandSmall_cSet) <- sensitivityInfo(clevelandSmall_cSet)
#'
#' @param object The [`CoreSet`] to update
#' @param dimension [`character`] Optional name of the dimension to extract,
#'     either 'cells' or 'drugs'. Only used if the sensitivity slot contains a
#'     `LongTable` object instead of a `list`.
#' @param ... Additional arguments to the rowData or colData
#'     `LongTable` methods. Only used if the sensitivity slot contains a
#'     `LongTable` object instead of a `list`.
#' @param value A \code{data.frame} with the new sensitivity annotations
#'
#' @return Updated \code{CoreSet}
#'
#' @describeIn CoreSet Update the sensitivity experiment info
#' @export
setReplaceMethod("sensitivityInfo",
                 signature = signature(object="CoreSet",
                                       value="data.frame"),
                 function(object, dimension, ..., value) {
  if (is(sensitivitySlot(object), 'LongTable')) {
      stop(.errorMsg("Assignment for the sensitivityInfo slot is not allowed!"))
  } else {
    if (!missing(dimension))
      warning(.warnMsg('The dimension argument is only valid if the sensitivity',
        'slot contains a LongTable object. Ignoring function dimension and ...',
        ' parameters.'))
    object@sensitivity$info <- value
  }
})