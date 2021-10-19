#' @title LongTable class definition
#'
#' @description Define a private constructor method to be used to build a
#'   `LongTable` object.
#'
#' @slot rowData See Slots section.
#' @slot colData See Slots section.
#' @slot assays See Slots section.
#' @slot metadata See Slots section.
#' @slot .intern See Slots section.
#' 
#' @section Slots:
#' - *rowData*: A `data.table` containing the metadata associated with the
#'   row dimension of a `LongTable`.
#' - *colData*: A `data.table` containing the metadata associated with the
#'   column dimension of a `LongTable`.
#' - *assays*: A `list` of `data.table`s, one for each assay in a
#'   `LongTable`.
#' - *metadata*: An optional `list` of additional metadata for a `LongTable`
#'   which doesn't map to one of the dimensions.
#' - *.intern*: An `environment` that holds internal structural metadata
#'   about a LongTable object, such as which columns are required to key
#'   the object. An environment has been used to allow locking items, which
#'   can prevent accidental modification of a property required for the class
#'   to work.
#'
#' @return `LongTable` object containing the assay data from a treatment
#'   response experiment
#'
#' @md
#' @import data.table
#' @keywords internal
#' @rdname LongTable-class
#' @aliases .LongTable
#' @exportClass LongTable
.LongTable <- setClass("LongTable",
    slots=list(
        rowData='data.table',
        colData='data.table',
        assays='list',
        metadata='list',
        .intern='environment')
)
#' @export
setOldClass('long.table', S4Class='LongTable')

#' @title LongTable constructor method
#'
#' @rdname LongTable
#'
#' @description Builds a `LongTable` object from rectangular objects. The
#' `rowData` argument should contain row level metadata, while the `colData`
#' argument should contain column level metadata, for the experimental assays
#' in the `assays` list. The `rowIDs` and `colIDs` lists are used to configure
#' the internal keys mapping rows or columns to rows in the assays. Each list
#' should contain at minimum one character vector, specifying which columns
#' in `rowData` or `colData` are required to uniquely identify each row. An
#' optional second character vector can be included, specifying any metadata
#' columns for either dimension. These should contain information about each
#' row but NOT be required to uniquely identify a row in the `colData` or
#' `rowData` objects. Additional metadata can be attached to a `LongTable` by
#' passing a list to the metadata argument.
#'
#' @param rowData `data.table`, `data.frame`, `matrix` A table like object
#'   coercible to a `data.table` containing the a unique `rowID` column which
#'   is used to key assays, as well as additional row metadata to subset on.
#' @param rowIDs `character`, `integer` A vector specifying
#'   the names or integer indexes of the row data identifier columns. These
#'   columns will be pasted together to make up the rownames of the
#'   `LongTable` object.
#' @param colData `data.table`, `data.frame`, `matrix` A table like object
#'   coercible to a `data.table` containing the a unique `colID` column which
#'   is used to key assays, as well as additional column metadata to subset on.
#' @param colIDs `character`, `integer` A vector specifying
#'   the names or integer indexes of the column data identifier columns. These
#'   columns will be pasted together to make up the colnames of the
#'   `LongTable` object.
#' @param assays A `list` containing one or more objects coercible to a
#'   `data.table`, and keyed by rowID and colID corresponding to the rowID and
#'   colID columns in colData and rowData.
#' @param metadata A `list` of metadata associated with the `LongTable`
#'   object being constructed
#' @param keep.rownames `logical`, `character`
#'   Logical: whether rownames should be added as a column if coercing to a
#'   `data.table`, default is FALSE. If TRUE, rownames are added to the column
#'   'rn'.
#'   Character: specify a custom column name to store the rownames in.
#'
#' @return A `LongTable` object containing the data for a treatment response
#'   experiment and configured according to the rowIDs and colIDs arguments.
#'
#' @import data.table
#' @export
LongTable <- function(rowData, rowIDs, colData, colIDs, assays,
        metadata=list(), keep.rownames=FALSE) {

    # handle missing parameters
    isMissing <- c(rowData=missing(rowData), rowIDs=missing(rowIDs),
        colData=missing(colData), assays=missing(assays))

    if (all(isMissing)) {
        rowData <- data.table(rowKey=1)
        rowIDs <- "rowKey"
        colData <- data.table(colKey=1)
        colIDs <- "colKey"
        assays <- list(measurement=data.table(rowKey=1, colKey=1, metric=0.123))
        isMissing <- FALSE
    } else if (any(isMissing)) {
        stop(.errorMsg('\nRequired parameter(s) missing: ',
            names(isMissing)[isMissing], collapse='\n\t'))
    }

    # check parameter types and coerce or error
    if (!is(colData, 'data.table'))
        tryCatch({ 
            colData <- data.table(colData, keep.rownames=keep.rownames) 
        }, error=function(e)
            stop(.errorMsg("colData must be coercible to a data.frame!"))
        )

    if (!is(rowData, 'data.table'))
        tryCatch({ 
            rowData <- data.table(rowData, keep.rownames=keep.rownames) },
        error=function(e) 
            stop(.errorMsg('rowData must be coerceible to a data.frame!'))
        )

    isDT <- is.items(assays, FUN=is.data.table)
    isDF <- is.items(assays, FUN=is.data.frame) & !isDT
    if (!all(isDT))
        tryCatch({
            for (i in which(isDF)) 
                assays[[i]] <- data.table(assays[[i]], keep.rownames)
        }, error = function(e, assays) {
            message(e)
            types <- lapply(assays, typeof)
            stop(.errorMsg(
                '\nList items are types: ',
                types, '\nPlease ensure all items in the assays list are ',
                'coerceable to a data.frame!'), collapse=', ')
        })

    # Create the row and column keys for LongTable internal mappings
    # Currently skipping if row and col key already exist to prevent failed
    #   join when calling this function from subset,LongTable-method
    if (!('rowKey' %in% colnames(rowData)))
        rowData[, c('rowKey') := .GRP, by=rowIDs]
    if(!('colKey' %in% colnames(colData)))
        colData[, c('colKey') := .GRP, by=colIDs]

    # initialize the internals object to store private metadata for a LongTable
    internals <- new.env()

    # capture row interal metadata
    if (is.numeric(rowIDs) || is.logical(rowIDs)) rowIDs <- colnames(rowData)[rowIDs]
    if (!all(rowIDs %in% colnames(rowData)))
        stop(.errorMsg('\nRow IDs not in rowData: ',
            setdiff(rowIDs, colnames(rowData)), collapse=', '))
    internals$rowIDs <- rowIDs
    lockBinding('rowIDs', internals)
    internals$rowMeta <- setdiff(colnames(rowData[, -'rowKey']), rowIDs)
    lockBinding('rowMeta', internals)

    # capture column internal metadata
    if (is.numeric(colIDs) || is.logical(colIDs))
        colIDs <- colnames(colData)[colIDs]
    if (!all(colIDs %in% colnames(colData)))
        stop(.errorMsg('\nColumn IDs not in colData: ',
            setdiff(colIDs, colnames(colData)), collapse=', '))
    internals$colIDs <- colIDs
    lockBinding('colIDs', internals)
    internals$colMeta <- setdiff(colnames(colData[, -'colKey']), colIDs)
    lockBinding('colMeta', internals)

    # Reorder columns to match the keys, this prevents issues in unit tests
    # caused by different column orders.
    setcolorder(rowData, unlist(mget(c('rowIDs', 'rowMeta'), internals)))
    setcolorder(colData, unlist(mget(c('colIDs', 'colMeta'), internals)))

    ## Assemble  the pseudo row and column names for the LongTable
    ### TODO:: Is this the slow part of the constructor?
    .pasteColons <- function(...) paste(..., collapse=':')
    rowData[, `:=`(.rownames=mapply(.pasteColons, transpose(.SD))),
        .SDcols=internals$rowIDs]
    colData[, `:=`(.colnames=mapply(.pasteColons, transpose(.SD))),
        .SDcols=internals$colIDs]

    return(.LongTable(rowData=rowData, colData=colData, assays=assays, 
        metadata=metadata, .intern=internals))
}

# ---- Class unions for CoreSet slots
#' A class union to allow multiple types in a CoreSet slot
#'
#' @include LongTable-class.R
setClassUnion('list_or_LongTable', c('list', 'LongTable'))

# #' Ensure that all rowID and colID keys are valid
# #'
# #' @param rowData A `data.table` containing row level annotations.
# #' @param colData A `data.table` containing column level annotations for a
# #'   `LongTable`.
# #' @param assays A `list` of `data.table`s, one for each assay in an
# #'   `LongTable`.
# #'
# #' @keywords internal
### FIXME:: Finish this and implement class validity methods for LongTable!
#.verifyKeyIntegrity <- function(rowData, colData, assays) {
#    if (!('rowKey' %in% colnames(rowData)) || !is.numeric(rowData$rowID))
#        message(blue('The rowKey column is missing from rowData! Please try
#            rebuilding the LongTable object with the constructor.'))
#    if (!('colKey' %in% colnames(colData)) || !is.numeric(colData$colID))
#        stop()
#}

# ---- LongTable Class Methods

#' @include allGenerics.R
NULL

## NOTE:: Issues printing are caused by ggplot::%+% over riding crayon::%+%
#' Show method for the LongTable class
#'
#' @examples
#' show(merckLongTable)
#'
#' @param object A `LongTable` object to print the results for.
#'
#' @return `invisible` Prints to console.
#'
#' @importFrom crayon %+% yellow red green blue cyan magenta
#' @import data.table
#' @export
setMethod('show', signature(object='LongTable'), function(object) {

    ## FIXME:: Function too long. Can I refactor to a helper that prints each slot?

    # ---- class descriptions
    cat(yellow$bold$italic('< LongTable >', '\n'))
    cat(yellow$bold('dim: ', .collapse(dim(object)), '\n'))

    # --- assays slot
    assayLength <- length(assays(object))
    assaysString <- paste0('assays(', assayLength, '): ')
    assayNames <- assayNames(object)
    assayNamesString <-
        if (length(assayNames(object)) > 6) {
            paste0(.collapse(head(assayNames, 3), ' ... ', .collapse(tail(assayNames, 3))))
        } else {
            .collapse(assayNames(object))
        }
    cat(yellow$bold(assaysString) %+% red(assayNamesString), '\n')

    # --- rownames
    rows <- nrow(rowData(object))
    rowsString <- paste0('rownames(', rows, '): ')
    rowNames <- rownames(object)
    rownamesString <-
        if (length(rowNames) > 6) {
            paste0(.collapse(head(rowNames, 2)), ' ... ', .collapse(tail(rowNames, 2)))
        } else {
            .collapse(rowNames)
        }
    cat(yellow$bold(rowsString) %+% green(rownamesString), '\n')

    # ---- rowData slot
    rowCols <- ncol(rowData(object))
    rowDataString <- paste0('rowData(', rowCols, '): ')
    rowColnames <- colnames(rowData(object))
    rowDataNamesString <-
        if (length(rowColnames) > 6) {
            paste0(.collapse(head(rowColnames, 3)), ' ... ', .collapse(tail(rowColnames, 3)))
        } else {
            .collapse(rowColnames)
        }
    cat(yellow$bold(rowDataString) %+% green(rowDataNamesString), '\n')

    # ---- colnames
    cols <- nrow(colData(object))
    colsString <- paste0('colnames(', cols, '): ')
    colnames <- colnames(object)
    colnamesString <-
        if (length(colnames) > 6) {
            paste0(.collapse(head(colnames, 3)), ' ... ', .collapse(tail(colnames, 3)))
        } else {
            .collapse(colnames)
        }
    cat(yellow$bold(colsString) %+% green(colnamesString), '\n')

    # ---- colData slot
    colCols <- ncol(colData(object))
    colDataString <- paste0('colData(', colCols, '): ')
    colColnames <- colnames(colData(object))
    colDataNamesString <-
        if (length(colColnames) > 6) {
            paste0(.collapse(head(colColnames, 3)), ' ... ', .collapse(tail(colColnames, 3)))
        } else {
            .collapse(colColnames)
        }
    cat(yellow$bold(colDataString) %+% green(colDataNamesString), '\n')


    # --- metadata slot
    metadataString <- paste0('metadata(', length(metadata(object)), '): ')
    metadataNames <- names(metadata(object))
    metadataNamesString <-
        if (length(metadataNames) > 6) {
            paste0(.collapse(head(metadataNames, 3), ' ... ', .collapse(tail(metadataNames, 3))))
        } else if (length(metadataNames) >= 1) {
            .collapse(metadataNames)
        } else {
            'none'
        }
    cat(yellow$bold(metadataString) %+% green(metadataNamesString), '\n')
})


# ==== LongTable Accessor Methods

#' Get the id column names for the rowData slot of a LongTable
#'
#' @examples
#' rowIDs(merckLongTable)
#'
#' @param object A `LongTable` to get the rowData id columns for.
#' @param data `logical` Should the rowData for the id columns be returned
#' instead of the column names? Default is FALSE.
#' @param key `logical` Should the key column also be returned?
#'
#' @return A `character` vector of rowData column names if data is FALSE,
#' otherwise a `data.table` with the data from the rowData id columns.
#'
#' @rdname LongTable-class
#' @family LongTable-class
#' @family LongTable-accessors
#'
#' @import data.table
#' @export
setMethod('rowIDs', signature(object='LongTable'),
        function(object, data=FALSE, key=FALSE) {
    cols <- getIntern(object, 'rowIDs')
    if (key) cols <- c(cols, 'rowKey')
    if (data) rowData(object, key=key)[, ..cols] else cols
})

#' Get the id column names for the rowData slot of a LongTable
#'
#' @examples
#' rowMeta(merckLongTable)
#'
#' @describeIn LongTable Get the names of the non-id columns from rowData.
#'
#' @param object A `LongTable` to get the rowData metadata columns for.
#' @param data `logical` Should the rowData for the metadata columns be returned
#' instead of the column names? Default is FALSE.
#' @param key `logical` Should the key column also be returned? Default is FALSE
#'
#' @return A `character` vector of rowData column names if data is FALSE,
#' otherwise a `data.table` with the data from the rowData metadta columns.
#'
#' @import data.table
#' @export
setMethod('rowMeta', signature(object='LongTable'),
    function(object, data=FALSE, key=FALSE)
{
    cols <- getIntern(object, 'rowMeta')
    cols <- cols[!grepl('^\\.', cols)]
    if (key) cols <- c(cols, 'rowKey')
    if (data) rowData(object, key=key)[, ..cols] else cols
})

#' Get the id column names for the colData slot of a LongTable
#'
#' @examples
#' colIDs(merckLongTable)
#'
#' @describeIn LongTable Get the names of the columns in colData required to
#' uniquely identify each row.
#'
#' @param object A `LongTable` to get the colData id columns for.
#' @param data `logical` Should the colData for the id columns be returned
#' instead of the column names? Default is FALSE.
#' @param key `logical` Should the key column also be returned? Default is FALSE.
#'
#' @return A `character` vector of colData column names if data is FALSE,
#' otherwise a `data.table` with the data from the colData id columns.
#'
#' @import data.table
#' @export
setMethod('colIDs', signature(object='LongTable'),
    function(object, data=FALSE, key=FALSE) {

    cols <- getIntern(object, 'colIDs')
    if (key) cols <- c(cols, 'colKey')
    if (data) colData(object, key=TRUE)[, ..cols] else cols

})

#' Get the id column names for the colData slot of a LongTable
#'
#' @examples
#' colMeta(merckLongTable)
#'
#' @describeIn LongTable Get the names of the non-id columns in the colData
#'   `data.table`.
#'
#' @param object A `LongTable` to get the colData metadata columns for.
#' @param data `logical` Should the colData for the metadata columns be returned
#'   instead of the column names? Default is FALSE.
#' @param key `logical` Should the key column also be returned?
#'
#' @return A `character` vector of colData column names if data is FALSE,
#'   otherwise a `data.table` with the data from the colData metadta columns.
#'
#' @import data.table
#' @export
setMethod('colMeta', signature(object='LongTable'),
    function(object, data=FALSE, key=FALSE) {

    cols <- getIntern(object, 'colMeta')
    cols <- cols[!grepl('^\\.', cols)]
    if (key) cols <- c(cols, 'colKey')
    if (data) colData(object, key=TRUE)[, ..cols] else cols
})

#' Retrieve the value columns for the assays in a LongTable
#'
#' @examples
#' assayCols(merckLongTable)
#'
#' @describeIn LongTable Get a list of column names for each assay in a
#'   `LongTable`.
#'
#' @param object `LongTable`
#' @param i Optional parameter specifying the `character` name or `integer`
#' index of the assay to get the column names for. If missing, returns a
#' list of value column names for all the assays.
#'
#' @return A `list` of `character` vectors containing the value column names for
#' each assay if i is missing, otherwise a `character` vector of value column
#' names for the selected assay.
#'
#' @import data.table
#' @export
setMethod('assayCols', signature(object='LongTable'),
    function(object, i) {

    colNameList <- lapply(assays(object, key=FALSE), names)
    if (!missing(i)) {
        if (length(i) > 1) stop(.errorMsg('The i parameter only accepts a ',
            'single assay name or index'))

        if ((is.numeric(i) && i < length(colNameList)) ||
            (is.character(i) && i %in% names(colNameList)))
            colNameList[[i]]
        else
            stop(.errorMsg("The specified index is invalid!"))
    } else {
        colNameList
    }
})

#' Retrieve the unique identifier columns used for primary keys in rowData and
#'    colData.
#'
#' @examples
#' idCols(merckLongTable)
#'
#' @param object `LongTable`
#'
#' @return `character` A character vector containing the unique rowIDs and
#'   colIDs in a LongTable object.
#'
#' @export
setMethod('idCols', signature('LongTable'),
    function(object) {
    return(unique(c(rowIDs(object), colIDs(object))))
})
