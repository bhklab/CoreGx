# ==== LongTable Class

#' Retrieve the row metadata table from a LongTable object
#'
#' @examples
#' rowData(merckLongTable)
#'
#' @describeIn LongTable Get the row level annotations for a `LongTable` object.
#'
#' @param x A [`LongTable`] object to retrieve the row metadata from.
#' @param key `logical` Should the rowKey column also be returned? Defaults
#'     to FALSE.
#' @param use.names `logical` This parameter is just here to stop matching
#'     the key argument to use.names from the rowData generic. It doesn't do
#'     anything at this time and can be ignored.
#'
#' @return A [`data.table`] containing rowID, row identifiers, and row metadata.
#'
#' @import data.table
#' @export
setMethod('rowData', signature(x='LongTable'), function(x, key=FALSE, use.names=FALSE) {
    return(if (key) copy(x@rowData[, -'.rownames']) else
        copy(x@rowData[, -c('.rownames', 'rowKey')]))
})

#' Updates the `rowData` slot as long as the ID columns are not changed.
#'
#' @examples
#' rowData(merckLongTable) <- rowData(merckLongTable)
#'
#' @describeIn LongTable Update the row annotations for a `LongTable` object.
#'   Currently requires that all columns in rowIDs(longTable) be present in
#'   value.
#'
#' @param x A [`LongTable`] object to modify.
# ' @param join A `logical` vector. If `TRUE` and not all existing rowIDs are
# '   in the  `value` object, the function will attempt to a left join with
# '   the existing `rowData` in `x`.
#' @param value A [`data.table`] or [`data.frame`] to update the `rowData` of
#'   `x` with.
#'
#' @return A copy of the [`LongTable`] object with the `rowData`
#'   slot updated.
#'
#' @md
#' @importFrom crayon cyan magenta
#' @importFrom SummarizedExperiment `rowData<-`
#' @import data.table
#' @export
setReplaceMethod('rowData', signature(x='LongTable'), function(x, value) {

    # type check input
    if (is(value, 'data.frame'))
        value <- data.table(value)
    if (!is(value, 'data.table'))
        stop(.errorMsg('\n[CoreGx::rowData<-] Please pass a data.frame or ',
            'data.table to update the rowData slot. We recommend modifying the ',
            'object returned by rowData(x) then reassigning it with rowData(x) ',
            '<- newRowData'))

    # remove key column
    if ('rowKey' %in% colnames(value)) {
        value[, rowKey := NULL]
        warning(.warnMsg('\n[CoreGx::rowData<-] Dropping rowKey from replacement',
            ' value, this function will deal with mapping the rowKey',
            ' automatically.'))
    }

    # assemble information to select proper update method
    rowIDCols <- rowIDs(x)
    sharedRowIDCols <- intersect(rowIDCols, colnames(value))

    ## TODO:: Can I remove this line, looks like it does nothing?
    #metadataCols <- setdiff(sharedRowIDCols, colnames(rowData(x)))
    #sharedMetadataCols <- intersect(metadataCols, colnames(value))

    # error if all the rowID columns are not present in the new rowData
    equalRowIDs <- rowIDCols %in% sharedRowIDCols
    if (!all(equalRowIDs)) warning(.warnMsg('\n[CoreGx::rowData<-] The ID columns ',
        rowIDCols[!equalRowIDs], 'are not present in value. The function ',
        'will attempt to join with existing rowIDs, but this may fail!', 
        collapse=', '))

    rowIDs <- rowIDs(x, data=TRUE, key=TRUE)

    rowData <- rowIDs[unique(value), on=.NATURAL]
    setkeyv(rowData, 'rowKey')
    rowData[, .rownames := paste0(mget(..rowIDCols), collapse=':')]

    x@rowData <- rowData
    x
})