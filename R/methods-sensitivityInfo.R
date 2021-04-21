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
#' @importFrom MatrixGenerics colAlls
#' @importFrom data.table setkeyv merge.data.table `:=` setDF
#' @noRd
.rebuildInfo <- function(longTable) {

    # Extract the information needed to reconstruct the sensitivityInfo data.frame
    assayIndexDT <- assay(longTable, 1, key=TRUE)[, .(rowKey, colKey)]
    setkeyv(assayIndexDT, c('rowKey', 'colKey'))
    rowDataDT <- rowData(longTable, key=TRUE)
    setkeyv(rowDataDT, 'rowKey')
    colDataDT <- colData(longTable, key=TRUE)
    setkeyv(colDataDT, 'colKey')

    rowIDcols <- rowIDs(longTable)[!grepl('dose', rowIDs(longTable))]
    colIDcols <- colIDs(longTable)
    rownameCols <- c(rowIDcols, colIDcols)

    # join the tables into the original data
    infoDT <- merge.data.table(assayIndexDT, rowDataDT, all=TRUE)
    setkeyv(infoDT, 'colKey')
    infoDT <- merge.data.table(infoDT, colDataDT, all=TRUE)[, -c('rowKey', 'colKey')]

    # determine which columns map 1:1 with new identifiers and subset to those
    infoDT_first <- infoDT[, head(.SD, 1), by=rownameCols]
    infoDT_last <- infoDT[, tail(.SD, 1), by=rownameCols]
    keepCols <- names(which(apply(infoDT_first == infoDT_last, MARGIN=2, 
        FUN=all)))
    infoDT_sub <- unique(infoDT[, ..keepCols])

    # rebuld the rownames
    .paste_ <- function(x, y) paste(x, y, sep='_')
    .paste_colon <- function(x, y) paste(x, y, sep=':')
    infoDT_sub[, drugid := Reduce(.paste_colon, mget(..rowIDcols))]
    infoDT_sub[, cellid := Reduce(.paste_colon, mget(..colIDcols))]
    infoDT_sub[, exp_id := Reduce(.paste_, .(drugid, cellid))]

    # convert to data.frame
    setDF(infoDT_sub, rownames=infoDT_sub$exp_id)
    return(infoDT_sub)
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