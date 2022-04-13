#' List of `deprecated` or `defunct` methods in the `CoreGx` R package.
#'
#' @details
#' ## deprecated
#'
#'
#' ## defunct
#' `buildLongTable`: This function no longer works as building a `LongTable` or
#' `TreatmentResponseExperiment` now uses a `DataMapper` and the `metaConstruct`
#' method. See `vignette("")`
#'
#' @rdname CoreGx-deprecated
#' @aliases
#' CoreGx-defunct
#' @export
NULL


# ===== Deprecated









# ===== Defunct

# -- buildLongTableClass

# ==== LongTable Class

#' LongTable build method
#'
#' @describeIn LongTable Create a LongTable object from a single data.table or
#'   data.frame object.
#'
#' @param from `character` Path to the .csv file containing the data and
#'   metadata from which to build the `LongTable`.
#' @param colDataCols `list` List with two `character` vectors, the first
#'   specifying one or more columns to be used as column identifiers (e.g.,
#'   drug name columns) and the second containing any additional metadata
#'   columns related to the column identifiers. If you wish to rename any of
#'   these columns, assign the new names to their respective character vectors.
#' @param rowDataCols `list` List with two `character` vectors, the first
#'   specifying one or more columns to be used as cell identifiers (e.g.,
#'   cell-line name columns) and the second containing any additional metadata
#'   columns related to the cell identifiers. If you wish to rename any of
#'   these columns, assign the new names to their respective character vectors.
#' @param assayCols `list` A named list of character vectors specifying how to
#'   parse assay columns into a list of `data.table`s. Each list data.table
#'   will be named for the name of corresponding list item and contain the columns
#'   specified in the character vector of column names in each list item. If
#'   there are no names for assayCols, the assays will be numbered by instead.
#'
#' @return A `LongTable` object containing one or more assays, indexed by
#'   rowID and colID.
#'
#' @import data.table
#' @export
setMethod('buildLongTable', signature(from='data.frame'),
        function(from, rowDataCols, colDataCols, assayCols) {

    .Defunct("metaConstruct", msg="This method has been deprecated
        in favour of use of the LongTableDataMapper metadata object
        with the metaConstruct method to build a LongTable object!")

    # -- local helpers
    .unlist <- function(x) unlist(x, recursive=TRUE, use.names=FALSE)

    # -- handle missing params
    missingParams <- c(missing(rowDataCols), missing(colDataCols), missing(assayCols))
    if (any(missingParams))
        stop(.errorMsg('[CoreGx::buildLongTable,data.frame] The following',
            ' parameters are required:',
            .collapse(c('rowDataCols', 'colDataCols', 'assayCols')[missingParams])))

    # -- validate input and return useful messages if invalid
    ## TODO:: Check input parameters are valid

    # -- convert to data.table by reference
    if (!is.data.table(from))
        from <- data.table(from)

    # -- build drug and cell metadata tables and index by the appropriate ID
    colData <- unique(from[, .unlist(colDataCols), with=FALSE])
    setorderv(colData, colDataCols[[1]])  # order by id columns
    colData[, colKey := seq_len(.N)]
    rowData <- unique(from[, .unlist(rowDataCols), with=FALSE])
    setorderv(rowData, rowDataCols[[1]])  # order by id columns
    rowData[, rowKey := seq_len(.N)]

    # -- add the row and column ids to the value data
    assayData <- from[rowData, on=.unlist(rowDataCols)][colData, on=as.character(unlist(colDataCols))]
    rm(from)
    assayData[, as.character(unique(c(.unlist(rowDataCols), .unlist(colDataCols)))) := NULL]
    # row reason to prevent sort in join because key sorts
    setkey(assayData, rowKey, colKey)

    setkey(rowData, rowKey)
    setkey(colData, colKey)

    # -- rename columns, if necessary
    rowDataColnames <- lapply(rowDataCols, names)
    notNullRownames <- !vapply(rowDataColnames, FUN=is.null, FUN.VALUE=logical(1))
    if (any(notNullRownames))
        for (i in which(notNullRownames)) {
            setnames(rowData, rowDataCols[[i]], names(rowDataCols[[i]]))
            rowDataCols[[i]] <- names(rowDataCols[[i]])
        }

    colDataColnames <- lapply(colDataCols, names)
    notNullColnames <- !vapply(colDataColnames, FUN=is.null, FUN.VALUE=logical(1))
    if (any(notNullColnames))
        for (i in which(notNullColnames)) {
            setnames(colData, colDataCols[[i]], names(colDataCols[[i]]))
            colDataCols[[i]] <- names(colDataCols[[i]])
        }

    # -- drop colKey or rowKey from assayCols, since we are adding it back in the
    # next step
    ## TODO:: Add a check to see if the keys are there to avoid dropping/re-adding
    .drop.in <- function(x, y) x[!(x %in% y)]
    assayCols <- lapply(assayCols, .drop.in, y=c('colKey', 'rowKey'))

    # -- add the index columns to the different assay column vectors
    # this allows the .selectDataTable helper to be more general
    .prependToVector <- function(vector, values) c(values, vector)
    assayCols <- lapply(assayCols, FUN=.prependToVector, values=c('rowKey', 'colKey'))
    if (is.null(names(assayCols))) names(assayCols) <- paste0('assay', seq_along(assayCols))
    assays <- lapply(assayCols, .selectDataTable, DT=assayData)

    # -- remove the colname suffixes by reference from assays which had the same
    # colnames prior to joining into a single DT
    for (assay in assays) {
        setnames(assay, colnames(assay), gsub('\\._\\d+$', '', colnames(assay)))
    }

    return(LongTable(rowData=rowData, rowIDs=rowDataCols[[1]],
                     colData=colData, colIDs=colDataCols[[1]],
                     assays=assays))
})

#' LongTable build method from character
#'
#' @describeIn LongTable Create a LongTable object from a single .csv file
#'
#' @param from `character` Path to the .csv file containing the data and
#'   metadata from which to build the `LongTable`.
#' @param colDataCols `list` List with two `character` vectors, the first
#'   specifying one or more columns to be used as column identifiers (e.g.,
#'   drug name columns) and the second containing any additional metadata
#'   columns related to the column identifiers.
#' @param rowDataCols `list` List with two `character` vectors, the first
#'   specifying one or more columns to be used as cell identifiers (e.g.,
#'   cell-line name columns) and the second containing any additional metadata
#'   columns related to the cell identifiers.
#' @param assayCols `list` A named list of character vectors specifying how to
#'   parse assay columns into a list of `data.table`s. Each list data.table
#'   will be named for the name of corresponding list item and contain the columns
#'   specified in the character vector of column names in each list item.
#'
#' @return A `LongTable` object containing one or more assays, indexed by
#'   rowID and colID.
#'
#' @import data.table
#' @importFrom crayon magenta
#' @export
setMethod('buildLongTable', signature(from='character'),
          function(from, rowDataCols, colDataCols, assayCols)
{
    if (length(from) > 1)  # Call list subsetting method
        buildLongTable(as.list(from), rowDataCols, colDataCols, assayCols)
    if (!file.exists(from))
        stop(magenta$bold("The is no file at path: ", from, '. Please double
            check the location of the source file!'))

    # read in data
    tableData <- .freadNA(from)

    return(buildLongTable(from=tableData, rowDataCols, colDataCols, assayCols))
})

#' LongTable build method from list
#'
#' @describeIn Create a LongTable object from a list containing file paths,
#'   data.frames and data.tables.
#'
#' @examples
#' assayList <- assays(merckLongTable, withDimnames=TRUE)
#' rowDataCols <- list(rowIDs(merckLongTable), rowMeta(merckLongTable))
#' colDataCols <- list(colIDs(merckLongTable), colMeta(merckLongTable))
#' assayCols <- assayCols(merckLongTable)
#' longTable <- buildLongTable(from=assayList, rowDataCols, colDataCols, assayCols)
#'
#' @param from `list` A list containing any combination of character file paths,
#'  data.tables and data.frames which will be used to construct the LongTable.
#' @param colDataCols `list` List with two `character` vectors, the first
#'   specifying one or more columns to be used as column identifiers (e.g.,
#'   drug name columns) and the second containing any additional metadata
#'   columns related to the column identifiers.
#' @param rowDataCols `list` List with two `character` vectors, the first
#'   specifying one or more columns to be used as cell identifiers (e.g.,
#'   cell-line name columns) and the second containing any additional metadata
#'   columns related to the cell identifiers.
#' @param assayCols `list` A named list of character vectors specifying how to
#'   parse assay columns into a list of `data.table`s. Each list data.table
#'   will be named for the name of corresponding list item and contain the columns
#'   specified in the character vector of column names in each list item.
#'
#' @return A `LongTable` object constructed with the data in `from`.
#'
#' @import data.table
#' @importFrom crayon magenta cyan
#' @export
setMethod('buildLongTable', signature(from='list'),
        function(from, rowDataCols, colDataCols, assayCols) {

    # Prevent modify by reference for data.tables in list
    from <- copy(from)

    # local helpers
    ##FIXME:: This is exactly what the Map function is (an alias for mapply with
    ##   SIMPLIFY=FALSE)
    .mapply <- function(...) mapply(..., SIMPLIFY=FALSE)

    # preprocess from list
    isChar <- is.items(from, 'character')
    isDT <- is.items(from, FUN=is.data.table)
    isDF <- is.items(from, FUN=is.data.frame) & !isDT

    if (!all(isChar | isDT | isDF))
        stop(.errorMsg('\n[CoreGx::buildLongTable,list-method] List items at',
            ' indexes ', .collapse(which(!(isChar | isDT | isDF ))),
            ' are not character, data.table or data.frame.', collapse=', '))

    if (any(isChar)) from <- c(from[!isChar], lapply(from[isChar], FUN=.freadNA))
    if (any(isDF)) for (i in which(isDF)) from[[i]] <- data.table(from[[i]])

    # validate mappings
    ## TODO:: Ensure there is no case where joining on rowMeta or colMeta gives
    #  different results than just ids
    joinCols <- unique(unlist(c(rowDataCols, colDataCols)))
    dataColNames <- lapply(from, FUN=colnames)
    joinColsIn <- lapply(dataColNames, `%in%`, x=joinCols)
    hasAllIdCols <- unlist(lapply(joinColsIn, FUN=all))
    if (!all(hasAllIdCols)) {
        missingCols <- unique(unlist(.mapply(`[`, x=joinCols, i=joinColsIn)))
        stop(.errorMsg('[CoreGx::buildLongTable,list] Assay(s) ',
            .collapse(which(hasAllIdCols)), ' are missing one or more id ',
            'columns: ', .collapse(missingCols), collapse=', '))
    }

    # Set keys for faster joins
    for (i in seq_along(from)) setkeyv(from[[i]], cols=joinCols)

    # join assays into a single table
    DT <- from[[1]]
    from[[1]] <- NULL
    for (i in seq_along(from))
        DT <- merge.data.table(DT, from[[i]], suffixes=c('', paste0('._', i)))

    # fix assayCols if there are duplicate column names between assays
    # the join will append '._n' where n is the assay index - 1
    nonDataCols <- setdiff(colnames(DT), unique(c(unlist(rowDataCols), unlist(colDataCols))))
    assaySuffixCols <- lapply(paste0('\\._', seq_along(from)), grep, x=nonDataCols, value=TRUE)
    .length.gt.0 <- function(x) length(x) > 0
    hasSuffixes <- unlist(lapply(assaySuffixCols, FUN=.length.gt.0))
    duplicatedCols <- lapply(assaySuffixCols[hasSuffixes], gsub,
        pattern='\\._\\d+', replacement='')

    .which.in <- function(x, y) which(x %in% y)
    whichHasSuffixes <- which(hasSuffixes) + 1
    whichDuplicated <- .mapply(.which.in,
        x=assayCols[whichHasSuffixes], y=duplicatedCols)
    assayCols[whichHasSuffixes] <-
        .mapply(replace, x=assayCols[whichHasSuffixes],
            list=whichDuplicated, values=assaySuffixCols[hasSuffixes])

    # construct new LongTable
    buildLongTable(from=DT, rowDataCols, colDataCols, assayCols)
})


# ---- Helper Methods

#' fread with more default na.strings
#'
#' @keywords internal
#' @noRd
.freadNA <- function(...) {
    as.na <- unique(c(getOption('datatable.na.string'),
        c('NA', 'NULL', 'NaN', 'missing', 'None',
            'none', 'na', 'null', 'Null', 'Na')))
    fread(..., na.strings=as.na)
}


#' Select a set of column names from a data.table, returning a copy of the
#'   data.table with duplicate rows removed
#'
#' @param colNames `character` The column names to select from the data.table
#' @param DT `data.table`, `data.frame`, `matrix` An object coercible to a `data.table`.
#'   Please note rownames will be dropped by default.
#' @param keep.rownames `logical` or `character` Passed through to the data.table coercing if DT is not a
#'   `data.table`. If TRUE, rownames will be caputured in the `rn` column; if FALSE (default) rownames will
#'   be dropped; if `character`, rownames will be captured in a column with the same name.
#'
#' @return `data.table` Copy of `DT` containing only the specified columns, with duplicate rows removed.
#'
#' @import data.table
#' @keywords internal
#' @noRd
.selectDataTable <- function(colNames, DT, keep.rownames=FALSE) {
    # validate input
    if (!is.data.table(DT)) {
        tryCatch({
            DT <- data.table(DT, keep.rownames=keep.rownames)
        }, warning=function(w) {
            warning(w)
        }, error=function(e) {
            message(e)
            stop("Argument to DT parameter must be coercible to a data.table!")
        })
    }
    if (!is.character(colnames(DT))) stop("Currently only character column ids are supported!")
    missingColumns <- setdiff(colNames, colnames(DT))
    if (length(missingColumns) > 0)
        warning(paste0("There are no columns named ", paste0(missingColumns, collapse=", "), 'in DT.
            Continuing subset without these columns.'))

    # perform subset and copy to prevent modify by refence issues
    selectedDT <- copy(unique(DT[, .SD, .SDcols=colnames(DT) %in% colNames]))

    return(selectedDT)
}