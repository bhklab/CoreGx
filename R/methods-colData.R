# ==== LongTable

#' Retrieve the column metadata table from a LongTable object
#'
#' @examples
#' colData(merckLongTable)
#'
#' # Get the keys as well, mostly for internal use
#' colData(merckLongTable, key=TRUE)
#'
#' @describeIn LongTable Get the column level annotations for a LongTable
#'   object.
#'
#' @param x A `LongTable` to retrieve column metadata from.
#' @param key `logical` She the colKey column also be returned? Defaults to
#'     FALSE.
#'
#' @return A `data.table` containing row identifiers and metadata.
#'
#' @import data.table
#' @export
setMethod('colData', signature(x='LongTable'), function(x, key=FALSE) {
    return(if (key) copy(x@colData[, -'.colnames']) else
        copy(x@colData[, -c('.colnames', 'colKey')]))
})

#' Updates the `colData` slot as long as the ID columns are not changed.
#'
#' @examples
#' colData(merckLongTable) <- colData(merckLongTable)
#'
#' @describeIn LongTable Upadte the colData of a LongTable object. Currently
#'   requires that all of the colIDs(longTable) be in the value object.
#'
#' @param x A `LongTable` object to modify.
#' @param value A `data.table` or `data.frame` to update with. Must have
#'   all of the colIDs currently in the `LongTable` object in order to ensure
#'   assay key mappings are consistent.
#'
#' @return A copy of the `LongTable` object with the `colData`
#'   slot updated.
#'
#' @importFrom crayon cyan magenta
#' @importFrom SummarizedExperiment colData<-
#' @import data.table
#' @export
setReplaceMethod('colData', signature(x='LongTable'), function(x, value) {

    # type check input
    if (is(value, 'data.frame'))
        value <- data.table(value)
    if (!is(value, 'data.table'))
        stop(.errorMsg('\n[CoreGx::colData<-] Please pass a data.frame or ',
            'data.table to update the rowData slot. We recommend modifying the ',
            'object returned by colData(x) then reassigning it with colData(x) ',
            '<- newColData'))

    # remove key column
    if ('colKey' %in% colnames(value)) {
        value[, colKey := NULL]
        warning(.warnMsg('\n[CoreGx::colData<-] Dropping colKey from replacement',
            ' value, this function will deal with mapping the colKey',
            ' automatically.'))
    }
    colIDcols <- colIDs(x)

    existingColDataDT <- colData(x, key=TRUE)
    colDataDT <- unique(value)[existingColDataDT, on=.NATURAL]

    setkeyv(colDataDT, 'colKey')
    colDataDT[, .colnames := paste(mget(..colIDcols, collapse=':'))]

    x@colData <- colDataDT
    x
})