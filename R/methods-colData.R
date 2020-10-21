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
#' @param x A [`LongTable`] to retrieve column metadata from.
#' @param key [`logical`] She the colKey column also be returned? Defaults to
#'     FALSE.
#'
#' @return A [`data.table`] containing row identifiers and metadata.
#'
#' @import data.table
#' @export
setMethod('colData', signature(x='LongTable'), function(x, key=FALSE) {
    return(if (key) x@colData[, -'.colnames'] else
        x@colData[, -c('.colnames', 'colKey')])
})

#' Updates the `colData` slot as long as the ID columns are not changed.
#'
#' @examples
#' colData(merckLongTable) <- colData(merckLongTable)
#'
#' @describeIn LongTable Upadte the colData of a LongTable object. Currently
#'   requires that all of the colIDs(longTable) be in the value object.
#'
#' @param x A [`LongTable`] object to modify.
#' @param value A [`data.table`] or [`data.frame`] to update with. Must have
#'   all of the colIDs currently in the `LongTable` object in order to ensure
#'   assay key mappings are consistent.
#'
#' @return A copy of the [`LongTable`] object with the `colData`
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

    # assemble information to select proper update method
    colIDCols <- colIDs(x)
    sharedColIDCols <- intersect(colIDCols, colnames(value))

    ## TODO:: Can I remove this?
    #metadataCols <- setdiff(sharedColIDCols, colnames(colData(x)))
    #sharedMetadataCols <- intersect(metadataCols, colnames(value))

    ## FIXME:: Do I still want to throw this error?
    ## TODO:: Add parameter that allows modification of colID cols which errors if they change when FALSE

    # error if all the colID columns are not present in the new colData
    equalColIDs <- sharedColIDCols %in% colIDCols
    if (!all(equalColIDs)) stop(.errorMsg('\n[CoreGx::colData<-] The ID ',
        ' columns ', sharedColIDCols[!equalColIDs], 'are not present in value. ',
        'Currently this function only supports updates with the same colID ',
        'columns as the current colData!', collapse=', '))

    colIDs <- colIDs(x, data=TRUE, key=TRUE)

    colData <- colIDs[value, on=sharedColIDCols]

    ## TODO:: Refactor .pasteColons into utilities.R or make a function to update .colnames
    .pasteColons <- function(...) paste(..., collapse=':')
    colData[, `:=`(.colnames=mapply(.pasteColons, transpose(.SD))), .SDcols=sharedColIDCols]
    setkeyv(colData, 'colKey')

    x@colData <- colData
    x
})