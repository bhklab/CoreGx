# ==== LongTable Class

#' @include LongTableDataMapper-class.R
#' @include DataMapper-class.R
NULL

#' @title LongTable to data.table conversion
#' @name as
#'
#' @examples
#' as(merckLongTable, 'data.table')
#'
#' @description Coerce a LongTable into a `data.table`.
#'
#' @param from `LongTable` Object to coerce.
#' @param to `character` Class name to coerce to, currently only 'data.table'
#'   and 'data.frame' are supported
#'
#' @return A `data.table` with the data from a LongTable.
#' 
#' @import data.table
#' @export
setAs('LongTable', 'data.table', def=function(from) {

    # extract the assay data
    longTableData <- assays(from, withDimnames=FALSE, key=TRUE)

    # join assays into a single table
    DT <- longTableData[[1]]
    longTableData[[1]] <- NULL
    for (i in seq_along(longTableData))
        DT <- merge.data.table(DT, longTableData[[i]],
            suffixes=c('', paste0('._', i)), by=.EACHI)

    # extract assay columns
    assayCols <- assayCols(from)

    # fix assayCols if there are duplicate column names between assays
    # the join will append '._n' where n is the assay index - 1
    ## TODO:: Make this a helper since it is reused in multiple functions
    .greplAny <- function(...) any(grepl(...))
    .paste0IfElse <- function(vector, suffix, isIn=c('rowKey', 'colKey'))
        ifelse(vector %in% isIn, vector, paste0(vector, suffix))
    hasSuffixes <- unlist(lapply(paste0('._', seq_along(longTableData)),
        FUN=.greplAny, x=colnames(DT)))
    if (any(hasSuffixes)) {
        whichHasSuffixes <- which(hasSuffixes) + 1
        assayCols[whichHasSuffixes] <-
            Map(FUN=.paste0IfElse,
                vector=assayCols[whichHasSuffixes],
                suffix=paste0('._', seq_along(longTableData))[hasSuffixes]
            )
    }

    # join the row and column data
    DT <- merge.data.table(DT, rowData(from, key=TRUE), by='rowKey')
    DT <- merge.data.table(DT, colData(from, key=TRUE), by='colKey')
    setkeyv(DT, c('rowKey', 'colKey'))

    # drop interal key columns
    DT[, c('rowKey', 'colKey') := NULL]

    # organize the returned columns
    colOrder <- unique(c(setdiff(
        colnames(DT), unlist(assayCols)),
        unlist(assayCols)
    ))
    setcolorder(DT, colOrder)

    longTableMapper <- LongTableDataMapper(
        rowDataMap=list(rowIDs(from), rowMeta(from)),
        colDataMap=list(colIDs(from), colMeta(from)),
        assayMap=lapply(assayCols, FUN=setdiff, 
            y=c(idCols(from), rowMeta(from), colMeta(from))
        ),
        metadataMap=tryCatch({ lapply(metadata(from), names) },
            error=function(e) list())
    )
    attr(DT, 'longTableDataMapper') <- longTableMapper

    # return the data.table
    return(DT)
})
#' @title Coerce a LongTable into a `data.table`
#'
#' @description S3 version of coerce method for convenience.
#'
#' @param x `LongTable` to coerce to a `data.table`
#'
#' @return A `data.table` containing the data from the LongTable, as well
#'   as the `longTableDataMapper' attribute which contains the data needed to
#'   reverse the coercion.
#'
#' @export
as.data.table.long.table <- function(x) as(x, 'data.table')
#' @title Coerce a LongTable into a `data.frame`
#' @name as
#'
#' @description Currently only supports coercing to data.table or data.frame
#'
#' @param from `LongTable` Object to coerce.
#' @param to `character` Class name to coerce to, currently only 'data.table'
#'   and 'data.frame' are supported
#'
#' @return `data.table` containing the data from the LongTable, with the
#'   `longTableDataMapper' attribute containg the metadata needed to reverse
#'   the coercing operation.
#'
#' @importFrom data.table data.table setDF
#' @export
setAs('LongTable', 'data.frame', def=function(from) {
    DT <- as(from, 'data.table')
    setDF(DT)
    return(DT)
})

#' @title Coerce a LongTable to a data.frame
#'
#' @examples
#' as(merckLongTable, 'data.frame')
#'
#' @description S3 version of coerce method fro convenience.
#'
#' @param x `LongTable` to coerce to `data.frame`.
#' @param row.names An optional `character` vector of rownames. We do not
#'   recommend using this parameter, it is included for S3 method consistency
#'   with `as.data.frame`.
#' @param optional `logical` Is it optional for row and column names to be
#'   valid R names? If FALSE will use the make.names function to ensure the
#'   row and column names are valid R names. Defaults to TRUE.
#' @param ... Does nothing.
#'
#' @return `data.frame` containing the data from the LongTable, with the
#'   `longTableDataMapper' attribute containg the metadata needed to reverse
#'   the coercion operation.
#'
#' @importFrom data.table data.table
#' @export
as.data.frame.long.table <- function(x, row.names, optional=TRUE, ...) {
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
#' @name as
#'
#' @examples
#' dataTable <- as(merckLongTable, 'data.table')
#' print(attr(dataTable, 'longTableDataMapper')) # Method doesn't work without this
#' as(dataTable, 'LongTable')
#'
#' @description Coerce a data.table with the proper configuration attributes
#'   back to a LongTable
#'
#' @param from A `data.table` with the 'longTableDataMapper' attribute, containing
#'   three lists named assayCols, rowDataCols and colDataCols. This attribute is
#'   automatically created when coercing from a `LongTable` to a `data.table`.
#'
#' @return `LongTable` object configured with the longTableDataMapper
#'
#' @export
setAs('data.table', 'LongTable', def=function(from) {

    if (!('longTableDataMapper' %in% names(attributes(from))))
        stop(.errorMsg('[CoreGx::as,data.table,LongTable] Coercing from ',
            'data.table to LongTable only works if the longTableMapper ',
            'attribute has been set!'))

    longTableMapper<- attr(from, 'longTableDataMapper')

    requiredConfig <- c('assayMap', 'rowDataMap', 'colDataMap')
    hasRequiredConfig <- vapply(requiredConfig, 
        FUN=\(x, f) length(do.call(f, list(x)))[[1]] > 0,
        x=longTableMapper, FUN.VALUE=logical(1))
    if (!all(hasRequiredConfig))
        stop(.errorMsg('The longTableDataMapper object is missing data from',
            'the ', paste0(requiredConfig[!hasRequiredConfig], collapse=', '),
            ' slots! Check attributes(from).'))

    rawdata(longTableMapper) <- from
    return(metaConstruct(longTableMapper))
})
#' @name as.long.table
#' @title Coerce from data.table to LongTable
#'
#' @examples
#' dataTable <- as(merckLongTable, 'data.table')
#' print(attr(dataTable, 'longTableDataMapper')) # Method doesn't work without this
#' as.long.table(dataTable)
#'
#' @description Coerce a data.table with the proper configuration attributes
#'   back to a LongTable
#'
#' @param x A `data.frame` with the 'longTableDataMapper' attribute, containing
#'    three lists named assayCols, rowDataCols and colDataCols. This attribute is
#'    automatically created when coercing from a LongTable to a data.table.
#'
#' @return `LongTable` object configured with the longTableDataMapper
#' @export
as.long.table <- function(x) as(x, 'LongTable')


#' @name as
#' @title Coerce a SummarizedExperiment to a data.table
#'
#' @examples
#' SE <- molecularProfilesSlot(clevelandSmall_cSet)[[1]]
#' as(SE, 'data.table')
#'
#' @param from `SummarizedExperiment` object.
#'
#' @return `data.table` with long format of data in `from`
#'
#' @importFrom data.table as.data.table melt.data.table merge.data.table
#' @export
setAs(from='SummarizedExperiment', to='data.table', function(from) {
    # -- extract sample metadata
    colDT <- as.data.table(colData(from), keep.rownames='.sample')
    # -- extract feature metadata
    rowDT <- as.data.table(rowData(from), keep.rownames='.feature')
    # -- extract and process assays
    assayL <- assays(from)
    assayDtL <- lapply(assayL, as.data.table, keep.rownames='.feature')
    meltDtL <- lapply(assayDtL, melt, id.vars='.feature', 
        variable.name='.sample', variable.factor=FALSE)
    assayDT <- meltDtL[[1]][, .(.sample, .feature)]
    for (i in seq_along(meltDtL))
        assayDT[[names(assayL)[[i]]]] <- meltDtL[[i]][['value']]
    # -- merge into a single long format table
    DT <- merge.data.table(assayDT, colDT, by='.sample')
    DT <- merge.data.table(DT, rowDT, by='.feature')
    # -- add metadata
    metadata <- metadata(from)
    notS4 <- !vapply(metadata, isS4, logical(1))
    if (!all(notS4)) .warning('Dropped S4 metadata during coercion to data.table!')
    for (name in names(metadata)[notS4]) assayDT[[name]] <- metadata[[name]]
    return(DT)
})

#' @name as
#' @title Coerce a SummarizedExperiment to a data.frame
#' 
#' @examples 
#' SE <- molecularProfilesSlot(clevelandSmall_cSet)[[1]]
#' as(SE, 'data.frame')
#' 
#' @param from `SummarizedExperiment` object.
#' 
#' @return `data.frame` with long format of data in `from`.
#' 
#' @importFrom data.table as.data.table melt.data.table merge.data.table
#' @export
setAs(from='SummarizedExperiment', to='data.frame', function(from) {
    setDF(as(from, 'data.table'))
})