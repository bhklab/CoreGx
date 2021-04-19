#' sensitivityInfo getter
#'
#' Get the senstivity information DataFrame from a CoreSet object
#'
#' @examples
#' data(CCLEsmall)
#' sensInf <- sensitivityInfo(CCLEsmall)
#'
#' @param object The [`CoreSet`] to retrieve sensitivity experiment
#'     annotations from
#' @param dimension [`character`] Optional name of the dimension to extract,
#'     either 'cells' or 'drugs'. Only used if the sensitivity slot contains a
#'     `LongTable` object instead of a `list`.
#' @param ... [`pairlist`] Additional arguments to the rowData or colData
#'     `LongTable` methods. Only used if the sensitivity slot contains a
#'     `LongTable` object instead of a `list`.
#'
#' @return a [`DataFrame`] with the experiment info
#'
#' @describeIn CoreSet Return the drug dose sensitivity experiment info
#'
#' @include allGenerics.R
#'
#' @export
setMethod(sensitivityInfo, signature("CoreSet"),
    function(object, dimension, ...) {
  # case where sensitivity slot is a LongTable
  if (is(sensitivitySlot(object), 'LongTable')) {
      if (!missing(dimension)) {
          switch(dimension,
              cells={ return(colData(sensitivitySlot(object), ...)) },
              drugs={ return(rowData(sensitivitySlot(object), ...)) },
              assays={ return(assay(sensitivitySlot(object), 'assay_metadata')) },
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

#' Replicate the $info slot in the old sensitivity list from the new LongTable
#'   object
#'
#' @param longTable [`LongTable`]
#'
#' @keywords internal
#' @noRd
.rebuildInfo <- function(longTable) {

    # Extract the information needed to reconstruct the sensitivityRaw array
    if ('assay_metadata' %in% assayNames(longTable)) {
        meta <- assay(longTable, 'assay_metadata')
    } else {
        meta <- assays(longTable)[[1]][, .(rowKey, colKey)]
    }
    setkeyv(meta, c('rowKey', 'colKey'))
    rowData <- rowData(longTable, key=TRUE)
    rowData[, .(drug1id, drug2id, cellid)]
    setkeyv(rowData, 'rowKey')
    colData <- colData(longTable, key=TRUE)
    setkeyv(colData, 'colKey')

    # join the tables into the original data
    info <- merge.data.table(meta, rowData, all=TRUE)
    setkeyv(info, 'colKey')
    info <- merge.data.table(info, colData, all=TRUE)[, -c('rowKey', 'colKey')]
    rownames <- info$rn
    info[, rn := NULL]

    # convert to data.frame by reference, assigning rownames
    setDF(info, rownames=rownames)

    return(info)
}

#' sensitivityInfo<- setter
#'
#' Set the sensitivityInfo data.frame in a CoreSet object
#'
#' @describeIn CoreSet Update the metadata for the treatment response
#'     experiments in the sensitivity slot.
#'
#' @examples
#' data(CCLEsmall)
#' sensitivityInfo(CCLEsmall) <- sensitivityInfo(CCLEsmall)
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
#' @import data.table
#' @export
setReplaceMethod("sensitivityInfo",
                 signature(object="CoreSet", value="data.frame"),
                 function(object, dimension, ..., value) {

    if (is(sensitivitySlot(object), 'LongTable')) {
        # coerce to data.table
        if (!is.data.table(value)) value <- data.table(value, keep.rownames=TRUE)
        value[, drug_cell_rep := seq_len(.N), by=.(cellid, drugid)]
        if (missing(dimension)) {
            valueCols <- colnames(value)
            # get existing column names
            rowDataCols <- colnames(rowData(object@sensitivity))
            colDataCols <- colnames(colData(object@sensitivity))
            assayMetaCols <- colnames(assay(object@sensitivity,
                'experiment_metadata', withDimnames=TRUE, key=FALSE))
            # drop any value columns that don't already exist
            hasValueCols <- valueCols %in% c(rowDataCols, colDataCols, assayMetaCols)
            if (!all(hasValueCols))
                warning(.warnMsg('\n[CoreGx::sensitivityInfo<-] Dropping ',
                    'columns ', .collapse(valueCols[!hasValueCols]), ' from ',
                    'value. Currently this setter only allows modifying ',
                    'existing columns when @sensitivity is a LongTable. For ',
                    'more fine grained updates please use the dimension ',
                    'argument.',  collapse=', '))
            # update the object
            message("Doing rowData")
            rowData(object@sensitivity, ...) <-
                unique(value[, .SD, .SDcols=valueCols %in% rowDataCols])
            message("Doing colData")
            colData(object@sensitivity, ...) <-
                unique(value[, .SD, .SDcols=valueCols %in% colDataCols])
            message('Doing assay_metadata')
            assay(object@sensitivity, 'experiment_metadata') <-
                unique(value[, .SD, .SDcols=valueCols %in% assayMetaCols])
        } else {
            switch(dimension,
                drugs={ rowData(object@sensitivity, ...) <- value },
                cells={ colData(object@sensitivity, ...) <- value },
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