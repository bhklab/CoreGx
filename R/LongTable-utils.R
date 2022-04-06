#' @include LongTable-class.R LongTable-accessors.R
#' @importFrom checkmate assertClass assertDataFrame
NULL


#### CoreGx dynamic documentation
####
#### Warning: for dynamic docs to work, you must set
#### Roxygen: list(markdown=TRUE, r6=FALSE)
#### in the DESCRPTION file!


# ===================================
# Utility Method Documentation Object
# -----------------------------------




# ======================================
# Subset Methods
# --------------------------------------


##
## == subset


#' Subset a `LongTable` using <dim>Key column
#'
#' @param x `LongTable`
#' @param keys `integer` Inte
#' @param dim `character(1)` One of "row" or "col", corresponding to "rowKey" or
#'   "colKey" columns in rowData or colData.
#' @param reindex `logical(1)` Should index values be reset such that they
#'   are the smallest possible set of consecutive integers. Modifies the
#'   "rowKey", "colKey", and all assayKey columns. Initial benchmarks indicate
#'   rindex=FALSE save 35% of execution time.
#'
#' @details
#' Initial benchmarks indicate this is marginally faster than .subsetByIndex
#' when only using a single selector. ~7% faster when reindex=TRUE, ~4% faster
#' when reindex=FALSE. Probably not worth using, but I already wrote it.
#'
#' @noRd
.subsetByDimData <- function(x, keys, dim=c("row", "col"), reindex=FALSE) {
    # -- parse dimension to subset on
    dim <- match.arg(dim)
    dimData <- get(paste0(dim, "Data"), getNamespace("CoreGx"))
    setDimData <- get(paste0(dim, "Data<-"), getNamespace("CoreGx"))
    dimKey <- paste0(dim, "Key")
    # -- validate input
    assertClass(x, "LongTable")
    assertIntegerish(keys)
    keys <- sort(unique(keys))
    x <- copy(x)
    getIntern(x) <- new.env(getIntern(x), parent=emptyenv())  # copy object environment to prevent modify by reference
    # -- subset slots
    dData <- dimData(x, raw=TRUE)[keys, ]
    index <- getIntern(x, "assayIndex")
    index <- index[col %in% value, , env=list(col=dimKey, value=keys)]
    assays <- assays(x, withDimnames=FALSE)
    metaKeys <- c("rowKey", "colKey")
    for (i in seq_along(assays)) {
        setkeyv(assays[[i]], metaKeys)
        # join based subsets use binary-search, O(log(n)) vs O(n) for vector-scan
        # see https://rdatatable.gitlab.io/data.table/articles/datatable-keys-fast-subset.html
        assays[[i]] <- assays[[i]][index[, metaKeys, with=FALSE], ]
    }
    # -- optionally reindex the table
    if (reindex) {
        # update assay keys first
        for (i in seq_along(assays)) {
            aKey <- names(assays)[i]
            assays[[i]][, (aKey) := .I]
            index[assays[[i]],
                col := value,
                env=list(col=aKey, value=paste0("i.", aKey))
            ]
            setkeyv(assays[[i]], names(assays)[i])
        }
        # update rowKey and colKey
        newKey <- paste0(".", dimKey)
        dData[, newKey := .I, env=list(newKey=newKey)]
        index[dData, oldKey := newKey, env=list(newKey=newKey, oldKey=dimKey)]
        setkeyv(index, metaKeys)
        dData[, `:=`(oldKey=newKey, newKey=NULL),
            env=list(newKey=newKey, oldKey=dimKey)]
    }
    # -- update object and return
    # delete row-/colKeys by reference
    for (a in assays) a[, (metaKeys) := NULL]
    # raw=TRUE allows direct modification of slots
    setDimData(x, raw=TRUE, value=dData)
    assays(x, raw=TRUE) <- assays
    unlockBinding("assayIndex", getIntern(x))
    assign("assayIndex", index, envir=getIntern(x))
    lockBinding("assayIndex", getIntern(x))
    return(x)
}


#' Subset a `LongTable` using an "assayIndex" data.frame
#'
#' @param x `LongTable`
#' @param index `data.frame` Table with columns "rowKey", "colKey" and
#'   ".<assayName>", were <assayName> is the value for each `assayNames(x)`.
#'   Warning: rownames are dropped internally in coercion to `data.table`,
#' @param reindex `logical(1)` Should index values be reset such that they
#'   are the smallest possible set of consecutive integers. Modifies the
#'   "rowKey", "colKey", and all assayKey columns. Initial benchmarks indicate
#'   `reindex=FALSE` saves ~20% of both execution time and memory allocation. The
#'   cost of reindexing decreases the smaller your subet gets.
#'
#' @return `LongTable` subset according to the provided index.
#'
#' @noRd
.subsetByIndex <- function(x, index, reindex=FALSE) {
    # -- validate input
    assertClass(x, "LongTable")
    assertDataFrame(index)
    if (!is.data.table(index)) setDT(index)
    x <- copy(x)
    # -- subset slots
    rData <- rowData(x, raw=TRUE)[sort(unique(index$rowKey)), ]
    cData <- colData(x, raw=TRUE)[sort(unique(sort(index$colKey))), ]
    assays <- assays(x, withDimnames=FALSE)
    metaKeys <- c("rowKey", "colKey")
    setkeyv(index, metaKeys)
    for (i in seq_along(assays)) {
        setkeyv(assays[[i]], metaKeys)
        # join based subsets use binary-search, O(log(n)) vs O(n) for vector-scan
        # see https://rdatatable.gitlab.io/data.table/articles/datatable-keys-fast-subset.html
        assays[[i]] <- assays[[i]][index[, metaKeys, with=FALSE], ]
    }
    # -- optionally reindex the table
    if (reindex) {
        # update assay keys first
        for (i in seq_along(assays)) {
            aKey <- names(assays)[i]
            assays[[i]][, (aKey) := .I]
            index[assays[[i]],
                col := value,
                env=list(col=aKey, value=paste0("i.", aKey))
            ]
            setkeyv(assays[[i]], names(assays)[i])
        }
        # update rowKey and colKey
        rData[, .rowKey := .I]
        cData[, .colKey := .I]
        index[rData, rowKey := .rowKey]
        setkeyv(index, "colKey")
        index[cData, colKey := .colKey]
        setkeyv(index, metaKeys)
        rData[, `:=`(rowKey=.rowKey, .rowKey=NULL)]
        cData[, `:=`(colKey=.colKey, .colKey=NULL)]
    }
    # -- update object and return
    # delete row-/colKeys by reference
    for (a in assays) a[, (metaKeys) := NULL]
    # raw=TRUE allows direct modification of slots
    rowData(x, raw=TRUE) <- rData
    colData(x, raw=TRUE) <- cData
    assays(x, raw=TRUE) <- assays
    mutableIntern <- mutable(getIntern(x))
    mutableIntern$assayIndex <- index
    x@.intern <- immutable(mutableIntern)
    return(x)
}

#' Subset method for a LongTable object.
#'
#' Allows use of the colData and rowData `data.table` objects to query based on
#'  rowID and colID, which is then used to subset all value data.tables stored
#'  in the dataList slot.
#' This function is endomorphic, it always returns a LongTable object.
#'
#' @examples
#' # Character
#' subset(merckLongTable, 'ABT-888', 'CAOV3')
#' # Numeric
#' subset(merckLongTable, 1, c(1, 2))
#' # Logical
#' subset(merckLongTable, , colData(merckLongTable)$cellid == 'A2058')
#' # Call
#' subset(merckLongTable, drug1id == 'Dasatinib' & drug2id != '5-FU',
#'     cellid == 'A2058')
#'
#' @param x `LongTable` The object to subset.
#' @param i `character`, `numeric`, `logical` or `expression`
#'  Character: pass in a character vector of drug names, which will subset the
#'    object on all row id columns matching the vector.
#'  Numeric or Logical: these select based on the rowKey from the `rowData`
#'    method for the `LongTable`.
#'  Call: Accepts valid query statements to the `data.table` i parameter,
#'    this can be used to make complex queries using the `data.table` API
#'    for the `rowData` data.table.
#' @param j `character`, `numeric`, `logical` or `expression`
#'  Character: pass in a character vector of drug names, which will subset the
#'    object on all drug id columns matching the vector.
#'  Numeric or Logical: these select based on the rowID from the `rowData`
#'    method for the `LongTable`.
#'  Call: Accepts valid query statements to the `data.table` i parameter,
#'    this can be used to make complex queries using the `data.table` API
#'    for the `colData` data.table.
#' @param assays `character`, `numeric` or `logical` Optional list of assay
#'   names to subset. Can be used to subset the assays list further,
#'   returning only the selected items in the new LongTable.
#' @param reindex `logical` Should the col/rowKeys be remapped after subsetting.
#'   defaults to TRUE. For chained subsetting you may be able to get performance
#'   gains by setting to FALSE and calling reindex() manually after subsetting
#'   is finished.
#'
#' @return `LongTable` A new `LongTable` object subset based on the specified
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

    assayData <- lapply(assays(longTable, withDimnames=FALSE)[keepAssays],
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

    newLongTable <- if (reindex) reindex(newLongTable) else newLongTable

    return(newLongTable)
})


#' Convenience function for converting R code to a call
#'
#' This is used to pass through unevaluated R expressions into subset and
#'   `[`, where they will be evaluated in the correct context.
#'
#' @examples
#' .(cell_line1 == 'A2058')
#'
#' @param ... `pairlist` One or more R expressions to convert to calls.
#'
#' @return `call` An R call object containing the quoted expression.
#'
#' @export
. <- function(...) substitute(...)

# ---- subset LongTable helpers

#' Collapse vector of regex queries with | and replace * with .*
#'
#' @param queryString `character` Raw regex queries.
#'
#' @return `character` Formatted regex query.
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
#' @param variable `symbol` A symbol containing an R variable
#'
#' @return `character(1)` A string representation of the code necessary to
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
#' @param DT `data.table` Object with the columns rowID and colID, preferably
#'  as the key columns.
#' @param indexList `list` Two integer vectors, one indicating the rowIDs and
#'  one indicating the colIDs to filter the `data.table` on.
#'
#' @return `data.table` A copy of `DT` subset on the row and column IDs specified
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

##
## == [ method


#' [ LongTable Method
#'
#' Single bracket subsetting for a LongTable object. See subset for more details.
#'
#' This function is endomorphic, it always returns a LongTable object.
#'
#' @examples
#' # Character
#' merckLongTable['ABT-888', 'CAOV3']
#' # Numeric
#' merckLongTable[1, c(1, 2)]
#' # Logical
#' merckLongTable[, colData(merckLongTable)$cellid == 'A2058']
#' # Call
#' merckLongTable[
#'      .(drug1id == 'Dasatinib' & drug2id != '5-FU'),
#'      .(cellid == 'A2058'),
#'  ]
#'
#' @param x `LongTable` The object to subset.
#' @param i `character`, `numeric`, `logical` or `call`
#'  Character: pass in a character vector of drug names, which will subset the
#'    object on all row id columns matching the vector. This parameter also
#'    supports valid R regex query strings which will match on the colnames
#'    of `x`. For convenience, * is converted to .* automatically. Colon
#'    can be to denote a specific part of the colnames string to query.
#'  Numeric or Logical: these select based on the rowKey from the `rowData`
#'    method for the `LongTable`.
#'  Call: Accepts valid query statements to the `data.table` i parameter as
#'    a call object. We have provided the function .() to conveniently
#'    convert raw R statements into a call for use in this function.
#' @param j `character`, `numeric`, `logical` or `call`
#'  Character: pass in a character vector of drug names, which will subset the
#'      object on all drug id columns matching the vector. This parameter also
#'      supports regex queries. Colon can be to denote a specific part of the
#'      colnames string to query.
#'  Numeric or Logical: these select based on the rowID from the `rowData`
#'      method for the `LongTable`.
#'  Call: Accepts valid query statements to the `data.table` i parameter as
#'      a call object. We have provided the function .() to conveniently
#'      convert raw R statements into a call for use in this function.
#' @param assays `character` Names of assays which should be kept in the
#'   `LongTable` after subsetting.
#' @param ... Included to ensure drop can only be set by name.
#' @param drop `logical` Included for compatibility with the '[' primitive,
#'   it defaults to FALSE and changing it does nothing.
#'
#' @return A `LongTable` containing only the data specified in the function
#'   parameters.
#'
#' @export
setMethod('[', signature('LongTable'),
        function(x, i, j, assays, ..., drop=FALSE) {
    subset(x, i, j, assays)
})


##
## == [[ method'


#' [[ Method for LongTable Class
#'
#' Select an assay from within a LongTable object.
#'
#' @describeIn LongTable Get an assay from a LongTable object. This method
#'   returns the row and column annotations by default to make assignment
#'   and aggregate operations easiers.
#'
#' @examples
#' merckLongTable[['sensitivity']]
#'
#' @param x `LongTable` object to retrieve assays from
#' @param i `character` name or `integer` index of the desired assay.
#' @param withDimnames `logical` Should the row and column IDs be joined to
#'    the assay. Default is TRUE to allow easy use of group by arguments when
#'    performing data aggregation using the `data.table` API.
#' @param metadata `logical` Should the row and column metadata also
#'    be joined to the to the returned assay. Default is withDimnames.
#' @param keys `logical` Should the row and column keys also be returned?
#'    Defaults to !withDimnames.
#'
#' @importFrom crayon cyan magenta
#' @import data.table
#' @export
setMethod('[[', signature('LongTable'), function(x, i, withDimnames=TRUE,
        metadata=withDimnames, keys=!withDimnames) {
    funContext <- .S4MethodContext('[[', class(x))

    if (metadata && !withDimnames) {
        .warning('\nUnable to return metadata without dimnames, proceeding',
            ' as if withDimnames=TRUE.')
        withDimnames <- TRUE
    }

    if (length(i) > 1)
        .error('\nPlease only select one assay! To subset on multiple',
            'assays please see ?subset')

    if (keys) {
        .warning('\nIgnoring withDimnames and metadata arguments when',
            ' keys=TRUE.')
        return(assay(x, i))
    } else {
        if (withDimnames || metadata)
            return(assay(x, i, withDimnames, metadata))
        else
            return(assay(x, i)[, -c('rowKey', 'colKey')])
    }
})


#' `[[<-` Method for LongTable Class
#'
#' Just a wrapper around assay<- for convenience. See
#' `?'assay<-,LongTable,character-method'.`
#'
#' @param x A `LongTable` to update.
#' @param i The name of the assay to update, must be in `assayNames(object)`.
#' @param value A `data.frame`
#'
#' @examples
#' merckLongTable[['sensitivity']] <- merckLongTable[['sensitivity']]
#'
#' @return A `LongTable` object with the assay `i` updated using `value`.
#'
#' @export
setReplaceMethod('[[', signature(x='LongTable'), function(x, i, value) {
    assay(x, i) <- value
    x
})


##
## == $ method


#' Select an assay from a LongTable object
#'
#' @examples
#' merckLongTable$sensitivity
#'
#' @param x A `LongTable` object to retrieve an assay from
#' @param name `character` The name of the assay to get.
#'
#' @return `data.frame` The assay object.
#'
#' @export
setMethod('$', signature('LongTable'), function(x, name) {
    # error handling is done inside `[[`
    x[[name]]
})

#' Update an assay from a LongTable object
#'
#' @examples
#' merckLongTable$sensitivity <- merckLongTable$sensitivity
#'
#' @param x A `LongTable` to update an assay for.
#' @param name `character(1)` The name of the assay to update
#' @param value A `data.frame` or `data.table` to update the assay with.
#'
#' @return Updates the assay `name` in `x` with `value`, returning an invsible
#' NULL.
#'
#' @export
setReplaceMethod('$', signature('LongTable'), function(x, name, value) {
    # error handling done inside `assay<-`
    x[[name]] <- value
    x
})


# ======================================
# Reindex Methods
# --------------------------------------

##
## == reindex

#' Redo indexing for a LongTable object to remove any gaps in integer indexes
#'
#' After subsetting a LongTable, it is possible that values of rowKey or colKey
#'   could no longer be present in the object. As a result there the indexes
#'   will no longer be contiguous integers. This method will calcualte a new
#'   set of rowKey and colKey values such that integer indexes are the smallest
#'   set of contiguous integers possible for the data.
#'
#' @param object The `LongTable` object to recalcualte indexes (rowKey and
#'     colKey values) for.
#'
#' @return A copy of the `LongTable` with all keys as the smallest set of
#'     contiguous integers possible given the current data.
#'
#' @export
setMethod('reindex', signature(object='LongTable'), function(object) {
    # -- copy to prevent modify by reference
    object <- copy(object)
    # -- extract object data
    index <- mutable(getIntern(object)$assayIndex)
    rData <- rowData(object, raw=TRUE)
    cData <- colData(object, raw=TRUE)
    assays <- assays(object, withDimnames=FALSE)
    metaKeys <- c("rowKey", "colKey")
    setkeyv(index, metaKeys)
    # -- compute the new index
    # update assay keys first
    for (i in seq_along(assays)) {
        setkeyv(assays[[i]], metaKeys)
        aKey <- names(assays)[i]
        assays[[i]][, (aKey) := .I]
        index[assays[[i]],
            col := value,
            env=list(col=aKey, value=paste0("i.", aKey))
        ]
        setkeyv(assays[[i]], names(assays)[i])
    }
    # update rowKey and colKey
    rData[, .rowKey := .I]
    cData[, .colKey := .I]
    index[rData, rowKey := .rowKey]
    setkeyv(index, "colKey")
    index[cData, colKey := .colKey]
    setkeyv(index, metaKeys)
    rData[, `:=`(rowKey=.rowKey, .rowKey=NULL)]
    cData[, `:=`(colKey=.colKey, .colKey=NULL)]
    # -- update object and return
    # delete row-/colKeys by reference
    for (a in assays) a[, (metaKeys) := NULL]
    # raw=TRUE allows direct modification of slots, for developer use
    setkeyv(rData, "rowKey")
    setkeyv(cData, "colKey")
    rowData(object, raw=TRUE) <- rData
    colData(object, raw=TRUE) <- cData
    assays(object, raw=TRUE) <- assays
    mutableIntern <- mutable(getIntern(object))
    mutableIntern$assayIndex <- index
    object@.intern <- immutable(mutableIntern)
    return(object)
})


#' @keywords internal
#' @noRd
.extractIDData <- function(assayDataList, idCols, keyName) {
    idDT <- data.table()
    for (assay in assayDataList) {
        idDT <- unique(rbindlist(list(idDT, assay[, ..idCols])))
    }
    rm(assayDataList)
    idDT[, eval(substitute(keyName := seq_len(.N)))]
    setkeyv(idDT, keyName)
    return(idDT)
}


#' @keywords interal
#' @noRd
.joinDropOn <- function(DT1, DT2, on) {
    DT1[DT2, on=on][, -get('on')]
}