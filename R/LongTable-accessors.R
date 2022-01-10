# Navigating this file:
# - Slot section names start with ----
# - Method section names start with ==
# 
# As a result, you can use Ctrl + f to find the slot or method you are looking
# for quickly, assuming you know its name.
# 
# For example Ctrl + f '== molecularProfiles' would take you the molecularProfiles
# method, while Ctrl + f '---- molecularProfiles' would take you to the slot
# section.

#' @include LongTable-class.R allGenerics.R
NULL

#### CoreGx dynamic documentation
####
#### Warning: for dynamic docs to work, you must set
#### Roxygen: list(markdown = TRUE, r6=FALSE)
#### in the DESCRPTION file!

.local_class_4 <- 'LongTable'
.local_data_4 <- 'merckLongTable'


# =======================================
# Accessor Method Documentation Object
# ---------------------------------------


#' @noRd
.docs_LongTable_accessors <- function(...) .parseToRoxygen(
    "
    @title Accessing and modifying information in a `{class_}`

    @description
    Documentation for the various setters and getters which allow manipulation
    of data in the slots of a `{class_}` object.

    @return Accessors: See details.
    @return Setters: An updated `{class_}` object, returned invisibly.
    ",
    ...
)

#' @name LongTable-accessors
#' @eval .docs_LongTable_accessors(class_=.local_class_4)
#' @eval .parseToRoxygen("@examples data({data_})", data_=.local_data_4)
NULL


# ======================================
# Accessor Methods
# --------------------------------------


## ==================
## ---- .intern Slot
## ------------------


##
## == getIntern


#' Get the symbol(s) x from the object@.intern slot of a LongTable
#'
#' This is used as an alternative to R attributes for storing structural
#' metadata of an S4 objects.
#'
#' @examples
#' getIntern(merckLongTable, 'rowIDs')
#' getIntern(merckLongTable, c('colIDs', 'colMeta'))
#'
#' @describeIn LongTable Access structural metadata present within a
#'   LongTable object. This is mostly for developmer use.
#'
#' @param object `LongTable`
#' @param x `character` One or more symbol name strings to retrieve from
#'     the object@.intern environment.
#' @param ... `pairlist` Addtional arguments to get or mget inside of the
#'     function.
#'
#' @return value of x if length(x) == 1 else named list of values for all
#'     symbols in x
#'
#' @include LongTable-class.R
#' @export
setMethod('getIntern', signature(object='LongTable', x='character'),
    function(object, x, ...) {

    if (length(x) > 1)
        tryCatch({ mget(x, envir=object@.intern, ...) },
            error=function(e) {
                message(e); mget(x, envir=object@.intern) })
    else
        tryCatch({ get(x, envir=object@.intern, ...) },
            error=function(e) {
                message(e); get(x, envir=object@.intern) })
})
#' @describeIn LongTable Access all structural metadata present within a
#'   LongTable object. This is mostly for developmer use.
#'
#' @param object `LongTable`
#' @param x `missing` This argument is excluded from from the function call.
#'
#' @return A named `list` with all values in environment object@.intern coerced
#' to a list (and therefore copied).
#' 
#' @examples
#' getIntern(merckLongTable)
#'
#' @aliases getIntern,LongTable,missing-method
#' @export
setMethod('getIntern', signature(object='LongTable', x='missing'),
    function(object, x) object@.intern
)


## ==================
## ---- rowData Slot
## ------------------


#' Retrieve the row metadata table from a LongTable object
#'
#' @examples
#' rowData(merckLongTable)
#'
#' @describeIn LongTable Get the row level annotations for a `LongTable` object.
#'
#' @param x A `LongTable` object to retrieve the row metadata from.
#' @param key `logical` Should the rowKey column also be returned? Defaults
#'     to FALSE.
#' @param use.names `logical` This parameter is just here to stop matching
#'     the posotopoma; argument to use.names from the rowData generic. It
#'     doesn't do anything at this time and can be ignored.
#'
#' @return A `data.table` containing rowID, row identifiers, and row metadata.
#'
#' @importFrom data.table data.table copy
#' @export
setMethod('rowData', signature(x='LongTable'), 
        function(x, key=FALSE, use.names=FALSE) {
    return(
        if (key) copy(x@rowData[, -'.rownames']) else
        copy(x@rowData[, -c('.rownames', 'rowKey')])
    )
})

#' Updates the `rowData` slot as long as the ID columns are not changed.
#'
#' @examples
#' rowData(merckLongTable) <- rowData(merckLongTable)
#'
#' @describeIn LongTable Update the row annotations for a `LongTable` object.
#'   Currently requires that all columns in rowIDs(longTable) be present in
#'   value.
#'
#' @param x A `LongTable` object to modify.
# ' @param join A `logical` vector. If `TRUE` and not all existing rowIDs are
# '   in the  `value` object, the function will attempt to a left join with
# '   the existing `rowData` in `x`.
#' @param value A `data.table` or `data.frame` to update the `rowData` of
#'   `x` with.
#'
#' @return A copy of the `LongTable` object with the `rowData`
#'   slot updated.
#'
#' @md
#' @importFrom crayon cyan magenta
#' @importFrom SummarizedExperiment `rowData<-`
#' @importFrom data.table setDT
#' @export
setReplaceMethod('rowData', signature(x='LongTable'), function(x, value) {

    # type check input
    if (is(value, 'data.frame')) setDT(value)
    if (!is(value, 'data.table'))
        .error('\n[CoreGx::rowData<-] Please pass a data.frame or ',
            'data.table to update the rowData slot. We recommend modifying the',
            ' object returned by rowData(x) then reassigning it with rowData(x)',
            ' <- newRowData')

    # remove key column
    if ('rowKey' %in% colnames(value)) {
        value[, rowKey := NULL]
        .message('\n[CoreGx::rowData<-] Dropping rowKey from replacement',
            ' value, this function will deal with mapping the rowKey',
            ' automatically.')
    }

    # assemble information to select proper update method
    rowIDCols <- rowIDs(x)
    sharedRowIDCols <- intersect(rowIDCols, colnames(value))

    # error if all the rowID columns are not present in the new rowData
    equalRowIDs <- rowIDCols %in% sharedRowIDCols
    if (!all(equalRowIDs)) .warning('\n[CoreGx::rowData<-] The ID columns ',
        rowIDCols[!equalRowIDs], ' are not present in value. The function ',
        'will attempt to join with existing rowIDs, but this may fail!', 
        collapse=', ')

    rowIDs <- rowIDs(x, data=TRUE, key=TRUE)

    ## TODO:: Throw error if user tries to modify ID columns

    rowData <- rowIDs[unique(value), on=.NATURAL, allow.cartesian=FALSE]
    rowData[, rowKey := .I]
    rowData <- rowData[!duplicated(rowKey), ]
    setkeyv(rowData, 'rowKey')
    rowData[, .rownames := Reduce(.paste_colon, mget(rowIDCols))]

    ## TODO:: Add some sanity checks before returing

    x@rowData <- rowData
    x
})


## ==================
## ---- colData Slot
## ------------------



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
#' @param x A `LongTable` to retrieve column metadata from.
#' @param key `logical` She the colKey column also be returned? Defaults to
#'     FALSE.
#'
#' @return A `data.table` containing row identifiers and metadata.
#'
#' @import data.table
#' @export
setMethod('colData', signature(x='LongTable'), function(x, key=FALSE) {
    return(if (key) copy(x@colData[, -'.colnames']) else
        copy(x@colData[, -c('.colnames', 'colKey')]))
})

#' Updates the `colData` slot as long as the ID columns are not changed.
#'
#' @examples
#' colData(merckLongTable) <- colData(merckLongTable)
#'
#' @describeIn LongTable Upadte the colData of a LongTable object. Currently
#'   requires that all of the colIDs(longTable) be in the value object.
#'
#' @param x A `LongTable` object to modify.
#' @param value A `data.table` or `data.frame` to update with. Must have
#'   all of the colIDs currently in the `LongTable` object in order to ensure
#'   assay key mappings are consistent.
#'
#' @return A copy of the `LongTable` object with the `colData`
#'   slot updated.
#'
#' @importFrom crayon cyan magenta
#' @importFrom SummarizedExperiment colData<-
#' @importFrom data.table data.table setDT
#' @export
setReplaceMethod('colData', signature(x='LongTable'), function(x, value) {

    # type check input
    if (is(value, 'data.frame')) setDT(value)
    if (!is(value, 'data.table'))
        .error('\n[CoreGx::colData<-] Please pass a data.frame or ',
            'data.table to update the rowData slot. We recommend modifying the ',
            'object returned by colData(x) then reassigning it with colData(x) ',
            '<- newColData')

    # remove key column
    if ('colKey' %in% colnames(value)) {
        value[, colKey := NULL]
        .message('\n[CoreGx::colData<-] Dropping colKey from replacement',
            ' value, this function will deal with mapping the colKey',
            ' automatically.')
    }
    colIDcols <- colIDs(x)

    ## TODO:: Throw error if user tries to modify colIDs

    existingColDataDT <- colData(x, key=TRUE)
    colDataDT <- existingColDataDT[unique(value), on=.NATURAL,
            allow.cartesian=FALSE]
    colDataDT[, colKey := .I]
    colDataDT <- colDataDT[!duplicated(colKey), ]

    setkeyv(colDataDT, 'colKey')
    colDataDT[, .colnames := Reduce(.paste_colon, mget(colIDcols))]


    ## TODO:: Sanity checks that this works as expected

    x@colData <- colDataDT
    x
})

## ==================
## ---- assaySlot
## ------------------


##
## == assays


#' Return a list of `data.table` objects with the assay measurements from a
#'  `LongTable` object.
#'
#' @examples
#' assays(merckLongTable)
#'
#' @describeIn LongTable Get a list containing all the assays in a `LongTable`.
#'
#' @param x `LongTable` What to extract the assay data from.
#' @param withDimnames `logical` Should the returned assays be joined to
#'   the row and column identifiers (i.e., the pseudo dimnames of the object).
#' @param metadata `logical` Should row and column metadata also be joined
#'   to the returned assays. This is useful for modifying assays before
#'   reconstructing a new LongTable.
#' @param key `logical` Should the key columns also be returned? Defaults
#'   to !`withDimnames`.
#'
#' @return A `list` of `data.table` objects, one per assay in the object.
#'
#' @importMethodsFrom SummarizedExperiment assays
#' @import data.table
#' @export
setMethod('assays', signature(x='LongTable'),
        function(x, withDimnames=TRUE, metadata=withDimnames,
            key=!withDimnames) {
    return(structure(
        lapply(assayNames(x), FUN=assay, x=x, withDimnames=withDimnames,
            metadata=metadata, key=key),
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
#'   assays(longTable, withDimnames=TRUE) and the reassigning to the 
#'   `LongTable`.
#'
#' @param x A `LongTable` to modify the assays in.
#' @param value A `list` of `data.frame` or `data.table` objects, all of which
#'   contain the row and column identifiers and metadata.
#'
#' @return A copy of the `LongTable` with the assays modified.
#'
#' @importMethodsFrom SummarizedExperiment assays<-
#' @import data.table
#' @export
setReplaceMethod('assays', signature(x='LongTable', value='list'),
        function(x, value) {
    assay_names <- names(value)
    for (name in assay_names) {
        x[[name]] <- value[[name]]
    }
    return(x)
})


##
## == assay


#' Get an assay from a LongTable object
#'
#' @describeIn LongTable Retrieve an assay `data.table` object from the
#'   `assays` slot of a `LongTable` object.
#'
#' @examples
#' # Default annotations, just the key columns
#' assay(merckLongTable, 'sensitivity')
#' assay(merckLongTable, 1)
#'
#' # With identifiers joined
#' assay(merckLongTable, 'sensitivity', withDimnames=TRUE)
#'
#' # With identifiers and metadata
#' assay(merckLongTable, 'profiles', withDimnames=TRUE, metadata=TRUE)
#'
#' @param x `LongTable` The `LongTable` object to get the assay from.
#' @param i `integer` or `character` vector containing the index or name
#'   of the assay, respectively.
#' @param withDimnames `logical` Should the dimension names be returned
#'   joined to the assay. This retrieves both the row and column identifiers
#'   and returns them joined to the assay.
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
setMethod('assay', signature(x='LongTable'), function(x, i, withDimnames=FALSE,
        metadata=withDimnames, key=!withDimnames) {

    # validate input
    if (length(i) > 1)
        .error('\n[CoreGx::assay] Please specifying a single string ',
            'assay name or integer index. See assayNames(x) for available ',
            'assays.')

    keepAssay <- if (is.character(i)) which(assayNames(x) == i) else i
    if (length(keepAssay) < 1)
        stop(.errorMsg('\n[CoreGx::assay] There is no assay ', i,
            ' in this LongTable. Use assayNames(longTable) for a list',
            'of valid assay names.'))

    # extract the specified assay
    time <- Sys.time()
    assayData <- x@assays[[keepAssay]]

    # optionally join to rowData and colData
    if (withDimnames && !metadata) {
        assayData <- rowIDs(x, data=TRUE, key=TRUE)[assayData, on='rowKey']
        assayData <- colIDs(x, data=TRUE, key=TRUE)[assayData, on='colKey']
    } else if (withDimnames && metadata) {
        assayData <- rowData(x, key=TRUE)[assayData, on='rowKey']
        assayData <- colData(x, key=TRUE)[assayData, on='colKey']
    }
    time1 <- Sys.time()
    time1 - time

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
#' assay(merckLongTable, 'sensitivity') <-
#'      assay(merckLongTable, 'sensitivity', withDimnames=TRUE)
#' assay(merckLongTable, 'sensitivity') <- merckLongTable$sensitivity
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
#'   assay has only some of the row or column indentifiers and an assay,
#'   `i`, already exists in `x`, then try join=TRUE to attempt to join with
#'   existing data.
#'
#' @return `LongTable` With updated assays slot.
#'
#' @describeIn LongTable
#'
#' @md
#' @importMethodsFrom SummarizedExperiment assay<-
#' @importFrom data.table data.table fsetdiff setcolorder set setDT
#' @export
setReplaceMethod('assay', signature(x='LongTable', i='character'),
        function(x, i, value) {
    funContext <- CoreGx:::.S4MethodContext('assay', class(x), class(i))
    if (!is.data.frame(value)) .error(funContext, 'Only a data.frame or',
        ' data.table can be assiged to the assay slot!')

    if (length(i) > 1) .error(funContext, ' Only a single assay ',
        'name can be assiged with assay(x, i) <- value.')

    if (!is.data.table(value)) setDT(value)

    ## TODO:: Do we want to support mutating the row and column metadata?

    # Extract identifier columns
    idColumns <- idCols(x)
    valueColumns <- setdiff(colnames(value), idColumns)
    metaColumns <- unique(c(rowMeta(x), colMeta(x)))

    # Determine if assigning new assay or updating existing assay
    existingAssay <- i %in% assayNames(x)

    # Handle missing mandatory identifiers in rowData and colData
    missingRowCols <- setdiff(rowIDs(x), colnames(value))
    missingRowTypes <- vapply(rowData(x)[, ..missingRowCols], FUN=typeof,
        FUN.VALUE=character(1))
    missingColCols <- setdiff(colIDs(x), colnames(value))
    missingColTypes <- vapply(colData(x)[, ..missingColCols], FUN=typeof,
        FUN.VALUE=character(1))
    missingColumns <- c(missingRowCols, missingColCols)
    missingTypes <- c(missingRowTypes, missingColTypes)

    if (existingAssay && length(missingColumns) > 0) {
            tryCatch({
                value <- merge.data.table(
                    assay(x, i, withDimnames=TRUE, metadata=FALSE),
                    value,
                    on=intersect(idColumns, colnames(value)),
                    all.x=TRUE)
            }, error=function(e) .error(funContext, 'Failed to map missing ',
                'id columns via join with existing assay: ', e))
    } else {
        # Pad missing id columns and sort
        for (idx in seq_along(missingColumns))
            set(value, j=missingColumns[idx], value=as(NA, missingTypes[idx]))
        setcolorder(value, idColumns)
        # Add new identifiers to be to the metadata
        newRowData <- fsetdiff(value[, .SD, .SDcols=rowIDs(x)],
            rowData(x)[, .SD, .SDcols=rowIDs(x)]
        )
        rowData(x) <- rbind(rowData(x), newRowData,
            use.names=TRUE, fill=TRUE)
        newColData <- fsetdiff(value[, .SD, .SDcols=colIDs(x)],
            colData(x)[, .SD, .SDcols=colIDs(x)]
        )
        colData(x) <- rbind(colData(x), newColData,
            use.names=TRUE, fill=TRUE)
    }

    # Determine what joins need to be done
    hasKeyColumns <- all(c('rowKey', 'colKey') %in% colnames(value))
    hasIdColumns <- all(idColumns %in% colnames(value))
    hasMetaColumns <- all(metaColumns %in% colnames(value))

    # Join to fetch the key columns if they are missing
    if (hasIdColumns && !hasKeyColumns) {
        value <- colData(x, key=TRUE)[value, on=colIDs(x)]
        value <- rowData(x, key=TRUE)[value, on=rowIDs(x)]
        value <- value[, .SD, .SDcols=c('rowKey', 'colKey', valueColumns)]
    } else if (!hasKeyColumns) {
        stop("The assay has insufficient information to be retrieve",
            " the row or column keys. Please ensure either the rowKey",
            " and colKey columns are present, or all of idCols(x)!")
    }

    x@assays[[i]] <- value
    return(x)
})


##
## == assayNames


#' Retrieve the assay names from a `LongTable` object.
#'
#' @examples
#' assayNames(merckLongTable)
#'
#' @describeIn LongTable Return the names of the assays contained in a
#'   `LongTable`
#'
#' @param x A `LongTable` object to retrieve the assay names from.
#'
#' @return `character` Names of the assays contained in the `LongTable`.
#'
#' @importMethodsFrom SummarizedExperiment assayNames
#' @export
setMethod('assayNames', signature(x='LongTable'), function(x) {
    return(names(x@assays))
})


## ==================
## ---- metadata Slot
## ------------------


#' Getter method for the metadata slot of a `LongTable` object
#'
#' @param x The `LongTable` object from which to retrieve the metadata list.
#'
#' @return `list` The contents of the `metadata` slot of the `LongTable`
#'   object.
#'
#' @importMethodsFrom S4Vectors metadata
#' @export
setMethod('metadata', signature(x='LongTable'), function(x) {
    return(x@metadata)
})


#' Setter method for the metadata slot of a `LongTable` object
#'
#' @param x `LongTable` The LongTable to update
#' @param value `list` A list of new metadata associated with a `LongTable`
#'   object.
#'
#' @return `LongTable` A copy of the `LongTable` object with the `value` in
#'   the metadata slot.
#'
#' @importMethodsFrom S4Vectors metadata<-
#' @importFrom crayon cyan magenta
#' @export
setReplaceMethod('metadata', signature(x='LongTable'), function(x, value) {
    if (!is(value, 'list'))
        stop(magenta$bold('The `metadata` slot must be a list!'))
    x@metadata <- value
    return(x)
})