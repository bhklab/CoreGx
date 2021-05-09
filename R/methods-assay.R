#' Get an assay from a LongTable object
#'
#' @describeIn LongTable Retrieve an assay `data.table` object from the
#'   `assays` slot of a `LongTable` object.
#'
#' @examples
#' # Default annotations, just the key columns
#' assay(merckLongTable, 'viability')
#' assay(merckLongTable, 1)
#'
#' # With identifiers joined
#' assay(merckLongTable, 'viability', withDimnames=TRUE)
#'
#' # With identifiers and metadata
#' assay(merckLongTable, 'viability_summary', withDimnames=TRUE, metadata=TRUE)
#'
#' @param x [`LongTable`] The `LongTable` object to get the assay from.
#' @param i `integer` or `character` vector containing the index or name
#'   of the assay, respectively.
#' @param withDimnames `logical` Should the dimension names be returned
#'   joined to the assay. This retrieves both the row and column identifiers
#'   and returns them attached to the
#' @param metadata `logical` Should all of the metadata also be joined to
#'   the assay. This is useful when modifying assays as the resulting list
#'   has all the information needed to recreated the LongTable object.
#' @param key `logical` Should the key columns also be returned? Defaults to
#'   !withDimnames.
#'
#' @importMethodsFrom SummarizedExperiment assay
#' @importFrom crayon magenta cyan
#' @import data.table
#' @export
setMethod('assay',
          signature(x='LongTable'),
          function(x, i, withDimnames=FALSE, metadata=withDimnames, key=!withDimnames) {

    # validate input
    if (length(i) > 1)
        stop(.errorMsg('\n[CoreGx::assay] Please specifying a single character ',
            'assay name or integer index. See assayNames(x) for available assays.'))

    keepAssay <- if (is.character(i)) which(assayNames(x) == i) else i
    if (length(keepAssay) < 1)
        stop(.errorMsg('\n[CoreGx::assay] There is no assay ', i,
            ' in this LongTable. Use assayNames(longTable) for a list',
            'of valid assay names.'))

    # extract the specified assay
    assayData <- x@assays[[keepAssay]]

    # optionally join to rowData and colData
    if (withDimnames && !metadata) {
        assayData <- rowIDs(x, data=TRUE, key=TRUE)[assayData, on='rowKey']
        assayData <- colIDs(x, data=TRUE, key=TRUE)[assayData, on='colKey']
    } else if (withDimnames && metadata) {
        assayData <- rowData(x, key=TRUE)[assayData, on='rowKey']
        assayData <- colData(x, key=TRUE)[assayData, on='colKey']
    }

    # drop any duplicated columns to prevent issues in the setter methods,
    # actually drops any columns prefixed with i.
    duplicates <- grep('^i\\..*', colnames(assayData), value=TRUE)
    ## TODO:: Is there any situation where ignoring duplicated keys could break the object?
    ## TODO:: Maybe add equality test for duplicate columns?
    warnDuplicates <- setdiff(duplicates, c('i.drug_cell_rep', 'i.rowKey', 'i.colKey'))
    if (length(duplicates) > 0) {
        if (length(warnDuplicates) > 0)
            warning(.warnMsg('\n[CoreGx::assay] Dropping columns duplicated when ',
                'joining assays with from ', i, 'when joining with rowData and ',
                'colData: ', .collapse(warnDuplicates)))
        assayData <- assayData[, -duplicates, with=FALSE]
    }

    if (!key) assayData <- assayData[, -c('rowKey', 'colKey')]

    if (!withDimnames && metadata)
        warning(.warnMsg('\n[CoreGx::assay] Cannot use metadata=TRUE when',
            ' withDimnames=FALSE. Ignoring the metadata argument.'))

    assayData
})


#' Add or replace an assay in a LongTable by name or index
#'
#' @description Add or replace an assay in a LongTable by name. Currently
#'    this function only works when the assay has all columns in row and column
#'    data tables (i.e., when assays is retured withDimnames=TRUE). This will
#'    be fixed in future updates.
#'
#' @examples
#' assay(merckLongTable, 'viability') <- assay(merckLongTable, 'viability', withDimnames=TRUE)
#' assay(merckLongTable, 'viability') <- merckLongTable$viability
#'
#' @param x A `LongTable` to update.
#' @param i `integer` or `character` vector containing the index or name
#'   of the assay to update.
#' @param value 
#' A `data.frame` or `data.table` to update the assay data
#'   with. This must at minumum contain the row and column data identifier
#'   columns to allow correctly mapping the assay keys. We recommend modifying
#'   the results returned by assay(longTable, 'assayName', withDimnames=TRUE).
#'   For convenience, both the `[[` and `$` LongTable accessors return an assay
#'   with the dimnames and metadata already attached. In the case where your
#'   assay has only some of the row or column indentifiers and the an assay, `i`,
#'   already exists in `x`, then try join=TRUE to attempt to join with existing
#'   data.
#'
#' @return `LongTable` With updated assays slot.
#'
#' @describeIn LongTable
#'
#' @md
#' @importMethodsFrom SummarizedExperiment assay<-
#' @import data.table
#' @export
setReplaceMethod('assay', signature(x='LongTable', i='character'),
    function(x, i, value) 
{

    funContext <- CoreGx:::.S4MethodContext('assay', class(x), class(i))
    if (!is.data.frame(value)) .error(funContext, 'Only a data.frame or 
        data.table can be assiged to the assay slot!')

    if (length(i) > 1) .error(funContext, 'Only a single assay ',
        'name can be assiged with assay(x, i) <- value.')

    whichAssay <- which(i %in% assayNames(x))

    assayData <- assays(x, withDimnames=TRUE, metadata=TRUE)

    if (!is.data.table(value)) setDT(value)

    # extract the row and column values
    rowIDCols <- rowIDs(x, key=FALSE)
    colIDCols <- colIDs(x, key=FALSE)
    rowMetaCols <- rowMeta(x, key=FALSE)
    colMetaCols <- colMeta(x, key=FALSE)

    # check that all the id columns are present, if not try to join them
    idCols <- unique(c(rowIDCols, colIDCols))
    hasIDCols <- idCols %in% colnames(value)
    if (!all(hasIDCols) && sum(hasIDCols >= 2)) {
            tryCatch({
                value <- merge.data.table(
                    assay(x, i, withDimnames=TRUE, metadata=FALSE), 
                    value, 
                    on=intersect(idCols, colnames(value)), 
                    all.x=TRUE)
            }, error=function(e) .error(funContext, 'Join failed for ID columns 
                with error: ', e))
    } else if (!all(hasIDCols)) {
        .error(funContext, 'Missing required id columns from value: ', 
            idCols[!hasIDCols], '. Please ensure you modify assay as returned 
            by assay(longTable, "<assayName>", withDimnames=TRUE, 
            metadata=TRUE).')
    }

    # check that all the metadata columns are present, if not try to join them
    metaCols <- unique(c(rowMetaCols, colMetaCols))
    hasMetaCols <- metaCols %in% colnames(value)
    if (!all(hasMetaCols)) {
        missingMetaCols <- metaCols[!hasMetaCols]
        tryCatch({
            value <- merge.data.table(
                assay(x, i, withDimnames=TRUE, metadata=TRUE)[, .SD, 
                    .SDcols=c(idCols, missingMetaCols)],
                value,
                on=idCols,
                all.x=TRUE
            )
        }, error=function(e) .error(funContext, 'Join failed for metadata 
            columns with error: ', e))
    }

    if (length(whichAssay) > 0) {
        assayData[[i]] <- value
    } else {
        assayData <- c(assayData, eval(str2lang(paste0('list(', i, '=value)'))))
    }

    assays(x) <- assayData
    x
})