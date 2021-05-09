#' Return a list of `data.table` objects with the assay measurements from a
#'  `LongTable` object.
#'
#' @examples
#' assays(merckLongTable)
#'
#' @describeIn LongTable Get a list containing all the assays in a `LongTable`.
#'
#' @param x [`LongTable`] What to extract the assay data from.
#' @param withDimnames [`logical`] Should the returned assays be joined to
#'   the row and column identifiers (i.e., the pseudo dimnames of the object).
#' @param metadata [`logical`] Should row and column metadata also be joined
#'   to the returned assays. This is useful for modifying assays before
#'   reconstructing a new LongTable.
#' @param key [`logical`] Should the key columns also be returned? Defaults
#'   to !withDimnames.
#'
#' @return A [`list`] of `data.table` objects, one per assay in the object.
#'
#' @importMethodsFrom SummarizedExperiment assays
#' @import data.table
#' @export
##TODO:: Add key argument with default to FALSE to remove rowKey and colKey
setMethod('assays', signature(x='LongTable'),
    function(x, withDimnames=FALSE, metadata=withDimnames, key=!withDimnames) {

    return(structure(
        lapply(assayNames(x),
               FUN=assay,
               x=x, withDimnames=withDimnames, metadata=metadata, key=key),
               .Names=assayNames(x)))
})


#' Setter method for the assays slot in a LongTable object
#'
#' @examples
#' assays(merckLongTable) <- assays(merckLongTable, withDimnames=TRUE)
#'
#' @describeIn LongTable Update the assays in a LongTable object. The rowIDs
#'   and colIDs must be present in all assays to allow successfully remapping
#'   the keys. We recommend modifying the list returned by
#'   assays(longTable, withDimnames=TRUE) and the reassigning to the `LongTable`.
#'
#' @param x A [`LongTable`] to modify the assays in.
#' @param value A [`list`] of `data.frame` or `data.table` objects, all of which
#'   contain the row and column identifiers and metadata.
#'
#' @return A copy of the [`LongTable`] with the assays modified.
#'
#' @importMethodsFrom SummarizedExperiment assays<-
#' @import data.table
#' @export
setReplaceMethod('assays', signature(x='LongTable', value='list'), 
    function(x, value) 
{

    # check input is correct
    isDF <- is.items(value, 'data.frame')
    isDT <- is.items(value, 'data.table')
    if (!all(isDF))
        stop(.errorMsg('\n[CoreGx::assays<-] Items ', .collapse(which(!isDT)),
            ' in value are not data.tables or data.frames. These are the only ',
            'types allowed in the value argument!', collapse=', '))

    if (!all(isDT))
        for (i in which(!isDT)) values[[i]] <- data.table(values[[i]])

    # check new assay names
    if (is.null(names(value))) {
        warning(.warnMsg('\n[CoreGx::assays<-] The list being assigned to ',
            'assays has no names. Defaulting to numbered assays. You can ',
            'correct his with assayNames(value) <- names.'))
        names(value) <- paste0('assay', seq_along(value))
    }

    # extract the row and column values
    rowIDCols <- rowIDs(x, key=FALSE)
    colIDCols <- colIDs(x, key=FALSE)
    rowMetaCols <- rowMeta(x, key=FALSE)
    colMetaCols <- colMeta(x, key=FALSE)

    # check that all the id columns are present
    idCols <- unique(c(rowIDCols, colIDCols))
    assayCols <- lapply(value, colnames)
    hasIDCols <- lapply(assayCols, `%in%`, x=idCols)
    assayHasIDCols <- unlist(lapply(hasIDCols, all))
    if (!all(assayHasIDCols)) {
        assayMissingCols <- names(value)[!assayHasIDCols]
        missingCols <- idCols[unique(Reduce(c, lapply(hasIDCols, which)))]
        stop(.errorMsg('\n[CoreGx::assays<-] Assay(s) ', .collapse(assayMissingCols),
            , 'are missing one or more id cols: ', .collapse(missingCols),
            '. Please ensure you modify assays as returned by assays(longTable,',
            ' withDimnames=TRUE, metadata=TRUE).', collapse=', '))
    }

    ## TODO:: Should we support passing colKey and rowKey if the metadata columns are missing?
    ## TODO:: Could then use them to join with the rowData and colData?
    # Need to drop the keys because buildLongTable redoes indexing
    .drop_in <- function(x, y) x[!(x %in% y)]
    assayCols <- lapply(assayCols, .drop_in, y=c('colKey', 'rowKey'))
    nonAssayCols <- c(idCols, rowMetaCols, colMetaCols)
    assayCols <- lapply(assayCols, setdiff, nonAssayCols)

    # get the rowData and colData column mappings
    rowDataCols <- list(rowIDCols, rowMetaCols)
    colDataCols <- list(colIDCols, colMetaCols)

    # get assay column names
    allCols <- c(unlist(rowDataCols), unlist(colDataCols))
    assayCols <- lapply(assayCols, setdiff, y=allCols)
    names(assayCols) <- names(value)

    # rebuild the 
    .mergeOnNonAssayCols <- function(x, y) merge.data.table(x, y, by=nonAssayCols)
    valueDT <- Reduce(.mergeOnNonAssayCols, value)

    # reconstruct a new LongTable with the updated assay
    LTdataMapper <- LongTableDataMapper(rawdata=valueDT, 
        rowDataMap=rowDataCols, colDataMap=colDataCols, assayMap=assayCols)
    LT <- metaConstruct(LTdataMapper)

    # keep the metadata
    metadata(LT) <- metadata(x)
    return(LT)
})