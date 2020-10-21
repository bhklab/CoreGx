# ==== LongTable Class

#' @title LongTable to data.table conversion
#'
#' @examples
#' as(merckLongTable, 'data.table')
#'
#' as.data.table(merckLongTable)
#'
#' @description Coerce a LongTable into a `data.table`.
#'
#' @param from [`LongTable`] Object to coerce.
#' @param to [`character`] Class name to coerce to, currently only 'data.table'
#'   and 'data.frame' are supported
#'
#' @return A [`data.table`] with the data from a LongTable.
#'
#' @import data.table
#' @export
setAs('LongTable', 'data.table', def=function(from) {

    # local helpers
    .mapply <- function(...) mapply(..., SIMPLIFY=FALSE)

    # extract the assay data
    longTableData <- assays(from, key=TRUE)

    # join assays into a single table
    DT <- longTableData[[1]]
    longTableData[[1]] <- NULL
    for (i in seq_along(longTableData))
        DT <- merge.data.table(DT, longTableData[[i]], suffixes=c('', paste0('._', i)), by=.EACHI)

    # extract assay columns
    assayCols <- assayCols(from)

    # fix assayCols if there are duplicate column names between assays
    # the join will append '._n' where n is the assay index - 1
    ## TODO:: Make this a helper since it is reused in multiple functions
    .greplAny <- function(...) any(grepl(...))
    .paste0IfElse <- function(vector, suffix, isIn=c('rowKey', 'colKey'))
        ifelse(vector %in% isIn, vector, paste0(vector, suffix))
    hasSuffixes <- unlist(lapply(paste0('._', seq_along(longTableData)), .greplAny, x=colnames(DT)))
    if (any(hasSuffixes)) {
        whichHasSuffixes <- which(hasSuffixes) + 1
        assayCols[whichHasSuffixes] <-
            .mapply(FUN=.paste0IfElse,
                    vector=assayCols[whichHasSuffixes],
                    suffix=paste0('._', seq_along(longTableData))[hasSuffixes])
    }

    # join the row and column data
    DT <- merge.data.table(DT, rowData(from, key=TRUE))
    setkeyv(DT, c('rowKey', 'colKey'))
    DT <- merge.data.table(DT, colData(from, key=TRUE))

    # drop interal key columns
    DT[, c('rowKey', 'colKey') := NULL]

    # organize the returned columns
    colOrder <- c(setdiff(colnames(DT), unlist(assayCols)), unlist(assayCols))
    setcolorder(DT, colOrder)

    # capture configuration needed to reverse this operation
    ## TODO:: implement a configuration argument in constructor and get method for it.
    LongTable.config <- list(assayCols=assayCols,
                             rowDataCols=list(rowIDs(from), rowMeta(from)),
                             colDataCols=list(colIDs(from), colMeta(from)))
    attr(DT, 'LongTable.config') <- LongTable.config

    # return the data.table
    return(DT)
})
#' @title Coerce a LongTable into a `data.table`
#'
#' @description S3 version of coerce method for convenience.
#'
#' @param x [`LongTable`] to coerce to a `data.table`
#'
#' @return A [`data.table`] containing the data from the LongTable, as well
#'   as the `LongTable.config' attribute which contains the data needed to
#'   reverse the coercion.
#'
#' @export
as.data.table.LongTable <- function(x) as(x, 'data.table')
#' @title Coerce a LongTable into a `data.frame`
#'
#' @description Currently only supports coercing to data.table or data.frame
#'
#' @param from [`LongTable`] Object to coerce.
#' @param to [`character`] Class name to coerce to, currently only 'data.table'
#'   and 'data.frame' are supported
#'
#' @return [`data.table`] containing the data from the LongTable, with the
#'   `LongTable.config' attribute containg the metadata needed to reverse
#'   the coercing operation.
#'
#' @import data.table
#' @export
setAs('LongTable', 'data.frame', def=function(from) {
    DT <- as(from, 'data.table')
    setDF(DT)
    return(DT)
})

#' @title Coerce a LongTable to a data.frame
#' @name Coece method for LongTable
#'
#' @examples
#' as(merckLongTable, 'data.frame')
#' as.data.frame(merckLongTable)
#'
#' @description S3 version of coerce method fro convenience.
#'
#' @param x [`LongTable`] to coerce to `data.frame`.
#' @param row.names An optional [`character`] vector of rownames. We do not
#'   recommend using this parameter, it is included for S3 method consistency
#'   with `as.data.frame`.
#' @param optional [`logical`] Is it optional for row and column names to be
#'   valid R names? If FALSE will use the make.names function to ensure the
#'   row and column names are valid R names. Defaults to TRUE.
#' @param ... Does nothing.
#'
#' @return [`data.frame`] containing the data from the LongTable, with the
#'   `LongTable.config' attribute containg the metadata needed to reverse
#'   the coercion operation.
#'
#' @export
as.data.frame.LongTable <- function(x, row.names, optional=TRUE, ...) {
    DF <- as(x, 'data.frame')
    if (!missing(row.names)) {
        if (!is.character(x) || length(row.names) != nrow(DF))
            stop(.errorMsg('[CoreGx::as.data.frame.LongTable] The row.names ',
                'argument must be a character vector with length equal to ',
                nrow(DF)))
        if (!optional) {
            row.names <- make.names(row.names)
            colnames(DF) <- make.names(colnames(DF))
        }
        rownames(DF) <- row.names
    }
    DF
}


#' @title Coerce to data.table to LongTable
#'
#' @examples
#' dataTable <- as.data.table(merckLongTable)
#' print(attr(dataTable, 'LongTable.config')) # Method doesn't work without this
#'
#' as(dataTable, 'LongTable')
#' as.long.table(dataTable)
#'
#' @description Coerce a data.table with the proper configuration attributes
#'   back to a LongTable
#'
#' @param from A [`data.table`] with the 'LongTable.config' attribute, containing
#'   three lists named assayCols, rowDataCols and colDataCols. This attribute is
#'   automatically created when coercing from a `LongTable` to a `data.table`.
#'
#' @return [`LongTable`] object configured with the LongTable.config
#'
#' @export
setAs('data.table', 'LongTable', def=function(from) {

    if (!('LongTable.config' %in% names(attributes(from))))
        stop(.errorMsg('[CoreGx::as,data.table,LongTable] Coercing from ',
            'data.table to LongTable only works if the LongTable.config ',
            'attribute has been set!'))

    LongTable.config <- attr(from, 'LongTable.config')

    requiredConfig <- c('assayCols', 'rowDataCols', 'colDataCols')
    hasRequiredConfig <- requiredConfig %in% names(LongTable.config)
    if (!all(hasRequiredConfig))
        stop(.errorMsg('The LongTable.config attribute is missing the ',
            requiredConfig[!hasRequiredConfig], ' attribute(s).', collapse=', '))

    with(LongTable.config,
         buildLongTable(from, rowDataCols, colDataCols, assayCols))
})
#' @title Coerce from data.table to LongTable
#'
#' @description Coerce a data.table with the proper configuration attributes
#'   back to a LongTable
#'
#' @param x A [`data.frame`] with the 'LongTable.config' attribute, containing
#'    three lists named assayCols, rowDataCols and colDataCols. This attribute is
#'    automatically created when coercing from a LongTable to a data.table.
#'
#' @return [`LongTable`] object configured with the LongTable.config
#' @export
as.long.table <- function(x) as(x, 'LongTable')