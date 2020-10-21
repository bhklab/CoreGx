# ==== LongTable Class

#' Subset method for a LongTable object.
#'
#' Allows use of the colData and rowData `data.table` objects to query based on
#'  rowID and colID, which is then used to subset all value data.tables stored
#'  in the dataList slot.
#' This function is endomorphic, it always returns a LongTable object.
#'
#' @examples
#' # Character
#' subset(merckLongTable, 'CAOV3', 'ABT-888')
#' # Numeric
#' subset(merckLongTable, 1, c(1, 2))
#' # Logical
#' subset(merckLongTable, rowData(merckLongTable)$cell_line1 == 'A2058')
#' # Call
#' subset(merckLongTable, cell_line1 == 'A2058',
#'   drug1 == 'Dasatinib' & drug2 != '5-FU')
#'
#' @param x [`LongTable`] The object to subset.
#' @param i [`character`], [`numeric`], [`logical`] or [`expression`]
#'  Character: pass in a character vector of drug names, which will subset the
#'    object on all row id columns matching the vector.
#'  Numeric or Logical: these select based on the rowKey from the `rowData`
#'    method for the `LongTable`.
#'  Call: Accepts valid query statements to the `data.table` i parameter,
#'    this can be used to make complex queries using the `data.table` API
#'    for the `rowData` data.table.
#'
#' @param j [`character`], [`numeric`], [`logical`] or [`expression`]
#'  Character: pass in a character vector of drug names, which will subset the
#'    object on all drug id columns matching the vector.
#'  Numeric or Logical: these select based on the rowID from the `rowData`
#'    method for the `LongTable`.
#'  Call: Accepts valid query statements to the `data.table` i parameter,
#'    this can be used to make complex queries using the `data.table` API
#'    for the `colData` data.table.
#'
#' @param assays [`character`, `numeric` or `logical`] Optional list of assay
#'   names to subset. Can be used to subset the assays list further,
#'   returning only the selected items in the new LongTable.
#' @param reindex [`logical`] Should the col/rowKeys be remapped after subsetting.
#'   defaults to TRUE. For chained subsetting you may be able to get performance
#'   gains by setting to FALSE and calling reindex() manually after subsetting
#'   is finished.
#'
#' @return [`LongTable`] A new `LongTable` object subset based on the specified
#'      parameters.
#'
#' @importMethodsFrom BiocGenerics subset
#' @importFrom crayon magenta cyan
#' @import data.table
#' @export
setMethod('subset', signature('LongTable'), function(x, i, j, assays, reindex=TRUE) {

    longTable <- x
    rm(x)

    # local helper functions
    .rowData <- function(...) rowData(..., key=TRUE)
    .colData <- function(...) colData(..., key=TRUE)
    .tryCatchNoWarn <- function(...) suppressWarnings(tryCatch(...))
    .strSplitLength <- function(...) length(strsplit(...))

    # subset rowData
    ## FIXME:: Can I parameterize this into a helper that works for both row
    ## and column data?
    if (!missing(i)) {
        ## TODO:: Clean up this if-else block
        if (.tryCatchNoWarn(is.call(i), error=function(e) FALSE)) {
            rowDataSubset <- .rowData(longTable)[eval(i), ]
        } else if (.tryCatchNoWarn(is.character(i), error=function(e) FALSE)) {
            ## TODO:: Implement diagnosis for failed regex queries
            idCols <- rowIDs(longTable, key=TRUE)
            if (max(unlist(lapply(i, .strSplitLength, split=':'))) > length(idCols))
                stop(cyan$bold('Attempting to select more rowID columns than
                    there are in the LongTable.\n\tPlease use query of the form ',
                    paste0(idCols, collapse=':')))
            i <- grepl(.preprocessRegexQuery(i), rownames(longTable), ignore.case=TRUE)
            i <- str2lang(.variableToCodeString(i))
            rowDataSubset <- .rowData(longTable)[eval(i), ]
        } else {
            isub <- substitute(i)
            rowDataSubset <- .tryCatchNoWarn(.rowData(longTable)[i, ],
                error=function(e) .rowData(longTable)[eval(isub), ])
        }
    } else {
        rowDataSubset <- .rowData(longTable)
    }

    # subset colData
    if (!missing(j)) {
        ## TODO:: Clean up this if-else block
        if (.tryCatchNoWarn(is.call(j), error=function(e) FALSE, silent=TRUE)) {
            colDataSubset <- .colData(longTable)[eval(j), ]
        } else if (.tryCatchNoWarn(is.character(j), error=function(e) FALSE, silent=TRUE)) {
            ## TODO:: Implement diagnosis for failed regex queries
            idCols <- colIDs(longTable, key=TRUE)
            if (max(unlist(lapply(j, .strSplitLength, split=':'))) > length(idCols))
                stop(cyan$bold('Attempting to select more ID columns than there
                    are in the LongTable.\n\tPlease use query of the form ',
                    paste0(idCols, collapse=':')))
            j <- grepl(.preprocessRegexQuery(j), colnames(longTable), ignore.case=TRUE)
            j <- str2lang(.variableToCodeString(j))
            colDataSubset <- .colData(longTable)[eval(j), ]
        } else {
            jsub <- substitute(j)
            colDataSubset <- .tryCatchNoWarn(.colData(longTable)[j, ],
                error=function(e) .colData(longTable)[eval(jsub), ])
        }
    } else {
        colDataSubset <- .colData(longTable)
    }

    # Subset assays to only keys in remaining in rowData/colData
    rowKeys <- rowDataSubset$rowKey
    colKeys <- colDataSubset$colKey

    if (missing(assays)) { assays <- assayNames(longTable) }
    keepAssays <- assayNames(longTable) %in% assays

    assayData <- lapply(assays(longTable)[keepAssays],
                     FUN=.filterLongDataTable,
                     indexList=list(rowKeys, colKeys))

    # Subset rowData and colData to only keys contained in remaining assays
    ## TODO:: Implement message telling users which rowData and colData
    ## columns are being dropped when selecting a specific assay.
    assayRowIDs <- unique(unlist(lapply(assayData, `[`, j='rowKey', drop=TRUE)))
    assayColIDs <- unique(unlist(lapply(assayData, `[`, j='colKey', drop=TRUE)))

    rowDataSubset <- rowDataSubset[rowKey %in% assayRowIDs]
    colDataSubset <- colDataSubset[colKey %in% assayColIDs]

    newLongTable <- LongTable(colData=colDataSubset, colIDs=longTable@.intern$colIDs ,
                     rowData=rowDataSubset, rowIDs=longTable@.intern$rowIDs,
                     assays=assayData, metadata=metadata(longTable))

    return(if (reindex) reindex(newLongTable) else newLongTable)
})


## TODO:: Can we import this from one of the tidyverse pacakges?
#' Convenience function for converting R code to a call
#'
#' This is used to pass through unevaluated R expressions into subset and
#'   `[`, where they will be evaluated in the correct context.
#'
#' @examples
#' .(cell_line1 == 'A2058')
#'
#' @param ... [`parilist`] One or more R expressions to convert to calls.
#'
#' @export
. <- function(...) substitute(...)

# ---- subset LongTable helpers

#' Collapse vector of regex queries with | and replace * with .*
#'
#' @param queryString [`character`] Raw regex queries.
#'
#' @return [`character`] Formatted regex query.
#'
#' @keywords internal
#' @noRd
.preprocessRegexQuery <- function(queryString) {
    # Support vectors of regex queries
    query <- paste0(queryString, collapse='|')
    # Swap all * with .*
    query <- gsub('\\.\\*', '*', query)
    return(gsub('\\*', '.*', query))
}


#' @keywords internal
#' @noRd
.validateRegexQuery <- function(regex, names) {
    ## TODO:: return TRUE if reqex query is valid, otherwise return error message
}

#' Convert an R object in a variable into a string of the code necessary to
#'   create that object
#'
#' @param variable [`Symbol`] A symbol containing an R variable
#'
#' @return [`string`] A string representation of the code necessary to
#'   reconstruct the variable.
#'
#' @keywords internal
#' @noRd
.variableToCodeString <- function(variable) {
    codeString <- paste0(capture.output(dput(variable)), collapse='')
    codeString <- gsub('\"', "'", codeString)
    return(codeString)
}

#' Filter a data.table object based on the rowID and colID columns
#'
#' @param DT [`data.table`] Object with the columns rowID and colID, preferably
#'  as the key columns.
#' @param indexList [`list`] Two integer vectors, one indicating the rowIDs and
#'  one indicating the colIDs to filter the `data.table` on.
#'
#' @return [`data.table`] A copy of `DT` subset on the row and column IDs specified
#'  in `indexList`.
#'
#' @import data.table
#' @keywords internal
#' @noRd
.filterLongDataTable <- function(DT, indexList) {

    # validate input
    if (length(indexList) > 2)
        stop("This object is 2D, please only pass in two ID vectors, one for
             rows and one for columns!")

    if (!all(vapply(unlist(indexList), is.numeric, FUN.VALUE=logical(1))))
        stop('Please ensure indexList only contains integer vectors!')

    # extract indices
    rowIndices <- indexList[[1]]
    colIndices <- indexList[[2]]

    # return filtered data.table
    return(copy(DT[rowKey %in% rowIndices & colKey %in% colIndices, ]))
}