#' @include immutable-class.R
#' @include allGenerics.R
NULL

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
#' - *.intern*: An `immutable` `list` that holds internal structural metadata
#'   about a LongTable object, such as which columns are required to key
#'   the object.
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
        .intern='immutable_list')
)


#' @title LongTable constructor method
#'
#' @rdname LongTable
#'
#' @param rowData `data.frame` A rectangular object coercible to a `data.table`.
#' @param rowIDs `character` A vector of `rowData` column names needed to
#'   uniquely identify each row in a `LongTable`.
#' @param colData `data.frame` A rectangular object coercible to a `data.table.`
#' @param colIDs `chacter` A vector of `colData` column names needed to uniquely
#'   identify each column in a `LongTable`.
#' @param assays `list` A list of rectangular objects, each coercible to
#'   a `data.table`. Must be named and item names must match the `assayIDs`
#'   list.
#' @param assayIDs `list` A list of `character` vectors specifying the columns
#'   needed to uniquely identify each row in an `assay`. Names must match the
#'   `assays` list.
#' @param metadata `list` A list of one or more metadata items associated with
#'   a LongTable experiment.
#' @param keep.rownames `logical(1)` or `character(1)` Should rownames be
#'   retained when coercing to `data.table` inside the constructor. Default
#'   is FALSE. If TRUE, adds a 'rn' column to each rectangular object that
#'   gets coerced from `data.frame` to `data.table`. If a string, that becomes
#'   the name of the rownames column.
#'
#' @return A `LongTable` object containing the data for a treatment response
#'   experiment and configured according to the rowIDs and colIDs arguments.
#'
#' @examples
#' "See vignette('The LongTable Class', package='CoreGx')"
#'
#' @importFrom data.table key setkeyv
#' @export
LongTable <- function(rowData, rowIDs, colData, colIDs, assays, assayIDs,
        metadata=list(), keep.rownames=FALSE) {

    # handle missing parameters
    isMissing <- c(rowData=missing(rowData), rowIDs=missing(rowIDs),
        colIDs=missing(colIDs), colData=missing(colData), assays=missing(assays),
        assayIDs=missing(assayIDs))

    if (any(isMissing)) stop(.errorMsg('\nRequired parameter(s) missing: ',
        names(isMissing)[isMissing], collapse='\n\t'))

    # check parameter types and coerce or error
    if (!is(colData, "data.table")) {
        tryCatch({
            colData <- data.table(colData, keep.rownames=keep.rownames)
        }, error=function(e)
            stop(.errorMsg("colData must be coercible to a data.frame!"))
        )
    } else {
        colData <- copy(colData)
    }

    if (!is(rowData, "data.table")) {
        tryCatch({
            rowData <- data.table(rowData, keep.rownames=keep.rownames) },
        error=function(e)
            stop(.errorMsg("rowData must be coerceible to a data.frame!"))
        )
    } else {
        rowData <- copy(rowData)
    }

    isDT <- is.items(assays, FUN=is.data.table)
    isDF <- is.items(assays, FUN=is.data.frame) & !isDT
    if (!all(isDT)) {
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
    }
    assays <- copy(assays)

    ## FIXME:: Move all validity checks to top of the function to prevent wasted
    ## computation or into class validity method

    # capture row internal metadata
    if (is.numeric(rowIDs) || is.logical(rowIDs))
        rowIDs <- colnames(rowData)[rowIDs]
    if (!all(rowIDs %in% colnames(rowData)))
        stop(.errorMsg('\nRow IDs not in rowData: ',
            setdiff(rowIDs, colnames(rowData)), collapse=', '))

    # Create the row and column keys for LongTable internal mappings
    if (!('rowKey' %in% colnames(rowData)))
        rowData[, c('rowKey') := .GRP, keyby=c(rowIDs)]
    if (!('colKey' %in% colnames(colData)))
        colData[, c('colKey') := .GRP, keyby=c(colIDs)]

    # initialize the internals object to store private metadata for a LongTable
    # NOTE: assign parent as emptyenv to prevent leaving parent.frame on the stack
    internals <- setNames(vector("list", length=6),
        c("rowIDs", "rowMeta", "colIDs", "colMeta", "assayKeys", "assayIndex"))
    internals$rowIDs <- rowIDs
    internals$rowMeta <- setdiff(colnames(rowData[, -'rowKey']), rowIDs)

    # capture column internal metadata
    if (is.numeric(colIDs) || is.logical(colIDs))
        colIDs <- colnames(colData)[colIDs]
    if (!all(colIDs %in% colnames(colData)))
        stop(.errorMsg('\nColumn IDs not in colData: ',
            setdiff(colIDs, colnames(colData)), collapse=', '))
    internals$colIDs <- colIDs
    internals$colMeta <- setdiff(colnames(colData[, -'colKey']), colIDs)

    # -- capture assays internal metadata
    # sort such that rowIDs are first, then colIDs; ensures reindex returns
    # the same order as construtor
    for (i in seq_along(assayIDs)) {
        rids <- intersect(rowIDs, assayIDs[[i]])
        cids <- intersect(colIDs, assayIDs[[i]])
        assayIDs[[i]] <- c(rids, cids)
    }
    internals$assayKeys <- assayIDs

    # ensure names of assays and assayIDs match
    hasMatchingAssayNames <- names(assays) == names(assayIDs)
    if (!all(hasMatchingAssayNames)) stop(.errorMsg(
        "Mismatched names between assays and assayIDs for:\n\t",
        paste0(names(assays)[!hasMatchingAssayNames], collapse=", ")),
        call.=FALSE)
    # set keys for join with metadata
    for (nm in names(assays)) {
        setkeyv(assays[[nm]], assayIDs[[nm]])
        assays[[nm]][, (nm) := .I]
    }

    # build the index mapping assay rows to rowKey and colKey
    assayIndex <- expand.grid(rowKey=rowData$rowKey, colKey=colData$colKey)
    setDT(assayIndex)
    setkeyv(assayIndex, c("rowKey", "colKey"))
    setkeyv(rowData, "rowKey")
    assayIndex <- assayIndex[
        rowData[, c(rowIDs, "rowKey"), with=FALSE], ,
        on="rowKey"
    ]
    setkeyv(colData, "colKey")
    assayIndex <- assayIndex[
        colData[, c(colIDs, "colKey"), with=FALSE], ,
        on="colKey"
    ]
    setkeyv(assayIndex, c(rowIDs, colIDs))
    for (nm in names(assays)) {
        assayIndex[assays[[nm]], (nm) := get(nm)]
    }
    assayIndex[, (c(rowIDs, colIDs)) := NULL]
    validKeys <- assayIndex[
        which(rowAnys(!is.na(idx[, names(assays), with=FALSE]))),
        .(rowKey, colKey)
    ]
    assayIndex <- assayIndex[
        rowKey %in% unique(validKeys$rowKey) &
            colKey %in% unique(validKeys$colKey),
    ]
    setkeyv(assayIndex, names(assays))
    internals$assayIndex <- assayIndex

    # make internals immutable to prevent users from modifying structural metadata
    internals <- immutable(internals)

    # Drop extra assay columns and key by the assay key in the assay index
    for (i in seq_along(assays)) {
        assays[[i]][, (assayIDs[[i]]) := NULL]
        setkeyv(assays[[i]], names(assays)[i])
    }

    # Reorder columns to match the keys, this prevents issues in unit tests
    # caused by different column orders
    setkeyv(rowData, "rowKey")
    setkeyv(colData, "colKey")
    setcolorder(rowData, unlist(internals[c("rowIDs", "rowMeta")]))
    setcolorder(colData, unlist(internals[c('colIDs', 'colMeta')]))

    ## Assemble  the pseudo row and column names for the LongTable
    .pasteColons <- function(...) paste(..., collapse=':')
    rowData[, `:=`(.rownames=mapply(.pasteColons, transpose(.SD))),
        .SDcols=rowIDs]
    colData[, `:=`(.colnames=mapply(.pasteColons, transpose(.SD))),
        .SDcols=colIDs]
    return(CoreGx:::.LongTable(rowData=rowData, colData=colData, assays=assays,
        metadata=metadata, .intern=internals))
}

# ---- Class unions for CoreSet slots
#' A class union to allow multiple types in a CoreSet slot
#'
#' @include LongTable-class.R
setClassUnion('list_OR_LongTable', c('list', 'LongTable'))

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
    cat(yellow$bold$italic(paste0("<", class(object)[1], ">"), '\n'))
    cat(yellow$bold('dim: ', .collapse(dim(object)), '\n'))

    # --- assays slot
    assayLength <- length(assayNames(object))
    assaysString <- paste0('assays(', assayLength, '): ')
    assayNames <- assayNames(object)
    assayNamesString <- .collapse(assayNames(object))
    if (nchar(assayNamesString) > options("width")) {
        assayNamesString <- paste0(strwrap(assayNamesString), collapse="\n  ")
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
    cols <- mutable(getIntern(object, 'rowIDs'))
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
        function(object, data=FALSE, key=FALSE) {
    cols <- mutable(getIntern(object, 'rowMeta'))
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

    cols <- mutable(getIntern(object, 'colIDs'))
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

    cols <- mutable(getIntern(object, 'colMeta'))
    cols <- cols[!grepl('^\\.', cols)]
    if (key) cols <- c(cols, 'colKey')
    if (data) colData(object, key=TRUE)[, ..cols] else cols
})



#' Retrieve the unique identifier columns used for primary keys in rowData and
#'    colData.
#'
#' @describeIn LongTable Get the names of all id columns.
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

#' Retrieve a copy of the assayIndex from the `@.intern` slot.
#'
#' @describeIn LongTable Get the assayIndex item from the objects internal metadata.
#'
#' @param `x` A `LongTable` or inheriting class.
#'
#' @return A `mutable` copy of the "assayIndex" for `x`
#'
#' @examples
#' assayIndex(nci_TRE_small)
#'
#' @aliases assayIndex,LongTable-method
#' @export
setMethod("assayIndex", signature("LongTable"), function(x) {
    mutable(getIntern(x, "assayIndex"))
})

#' Retrieve a copy of the assayKeys from the `@.intern` slot.
#'
#' @describeIn LongTable Get the assayKeys item from the objects internal metadata.
#'
#' @param `x` A `LongTable` or inheriting class.
#' @param `i` An optional valid assay name or index in `x`.
#'
#' @return A `mutable` copy of the "assyKeys" for `x`
#'
#' @examples
#' assayKeys(nci_TRE_small)
#' assayKeys(nci_TRE_small, "sensitivity")
#' assayKeys(nci_TRE_small, 1)
#'
#' @aliases assayKeys,LongTable-method
#' @export
setMethod("assayKeys", signature("LongTable"), function(x, i) {
    keys <- mutable(getIntern(x, "assayKeys"))
    # error handling occurs in `[[`
    if (!missing(i)) keys[[i]] else keys
})


#' Retrieve the value columns for the assays in a LongTable
#'
#' @examples
#' assayCols(merckLongTable)
#'
#' @describeIn LongTable Get a list of column names for each assay in the object.
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
    if (!missing(i)) {
        stopifnot(is.numeric(i) || is.character(i))
        stopifnot(length(i) == 1)
        stopifnot(i %in% assayNames(object) ||
            i %in% seq_along(assayNames(object)))
    }
    keys <- assayKeys(object)
    assayColnames <- Map(setdiff,
        x=lapply(assays(object, raw=TRUE), FUN=colnames),
        y=as.list(assayNames(object))
    )
    assayCols <- Map(c, keys, assayColnames)
    if (!missing(i)) assayCols[[i]] else assayCols
})