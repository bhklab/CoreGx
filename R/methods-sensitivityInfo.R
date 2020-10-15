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
#'     either 'cells', 'drugs', 'experiments'. Only used if the sensitivity slot
#'     contains a `LongTable` object instead of a `list`.
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
setMethod(sensitivityInfo, "CoreSet", function(object, dimension, ...) {

  # case where sensitivity slot is a LongTable
  if (is(sensitivitySlot(object), 'LongTable')) {
      if (!missing(dimension)) {
          switch(dimension,
              cells={ return(rowData(sensitivitySlot(object), ...)) },
              drugs={ return(colData(sensitivitySlot(object), ...)) },
              stop(.errorMsg('\n[CoreGx::sensitivityInfo] Invalid value for ',
                  'the dimension argument. Please select on of "cells" or ' ,
                  '"drugs"')))
      } else {
          return(.rebuildInfo(sensitivitySlot(object)))
      }
  # sensitivity is a list
  } else {
      if (!missing(dimension))
          warning(.warnMsg('\n[CoreGx::sensitivityInfo] The dimension argument ',
              'is only valid if the sensitivity slot contains a LongTable object.',
                  ' Ignoring the dimension and ... parameters.'))
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
                 signature(object="CoreSet", value="data.frame"),
                 function(object, dimension, ..., value) {

    if (is(sensitivitySlot(object), 'LongTable')) {
        if (missing(dimension)) {
            # coerce to data.table
            if (!is.data.table(value)) setDT(value, keep.rownames=TRUE)
            value[, drug_cell_rep := seq_len(dim(.SD)[1]), by=.(cellid, drugid)]
            valueCols <- colnames(value)
            # get existing column names
            rowDataCols <- colnames(rowData(object@sensitivity))
            colDataCols <- colnames(colData(object@sensitivity))
            experimentCols <- colnames(assay(object@sensitivity,
                'experiment_metadata', withDimnames=TRUE, key=FALSE))
            # drop any value columns that don't already exist
            hasValueCols <- valueCols %in% c(rowDataCols, colDataCols, experimentCols)
            if (!all(hasValueCols))
                warning(.warnMsg('\n[CoreGx::sensitivityInfo<-] Dropping columns ',
                    valueCols[!hasValueCols], ' from value. Currently this setter',
                    ' only allows modifying existing columns when @sensitivity is',
                    ' a LongTable. For more fine grained updates please use the',
                    ' dimension argument.',  collapse=', '))
            # update the object
            message("Doing rowData")
            rowData(object@sensitivity, ...) <-
                unique(value[, .SD, .SDcols=valueCols %in% rowDataCols])
            message("Doing colData")
            colData(object@sensitivity, ...) <-
                unique(value[, .SD, .SDcols=valueCols %in% colDataCols])
            message('Doing experment_metadata')
            assay(object@sensitivity, 'experiment_metadata') <-
                unique(value[, .SD, .SDcols=valueCols %in% experimentCols])
        } else {
            switch(dimension,
                cells={ rowData(object@sensitivity, ...) <- value },
                drugs={ colData(object@sensitivity, ...) <- value },
                experiments={ assay(object@sensitivity, 'experiment_metadata') <- value },
                stop(.errorMsg('\n[CoreGx::sensitivityInfo<-] Invalid argument to',
                    'dimension parameter. Please choose one of "cells" or "drugsA"')))
        }
    } else {
        if (!missing(dimension))
            warning(.warnMsg('\n[CoreGx::sensitivityInfo<-] The dimension argument ',
                'is only valid if the sensitivity slot contains a LongTable object. ',
                'Ignoring dimension and ... parameters.'))
        object@sensitivity$info <- value
    }
    return(object)
})