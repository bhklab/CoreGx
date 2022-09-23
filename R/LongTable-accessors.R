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
#'
#' @return `immutable` value of x if length(x) == 1 else named list of values
#'     for all symbols in x.
#'
#' @include LongTable-class.R
#' @export
setMethod('getIntern', signature(object='LongTable', x='character'),
        function(object, x) {
    return(if (length(x) == 1) object@.intern[[x]] else object@.intern[x])
})
#' @describeIn LongTable Access all structural metadata present within a
#'   LongTable object. This is primarily for developmer use.
#'
#' @param object `LongTable`
#' @param x `missing` This argument is excluded from from the function call.
#'
#' @return An `immutable` list.
#'
#' @examples
#' getIntern(merckLongTable)
#'
#' @aliases getIntern,LongTable,missing-method
#' @export
setMethod('getIntern', signature(object='LongTable', x='missing'),
    function(object, x) object@.intern
)

#' Set the .intern slot of a LongTable
#'
#' @param object `LongTable`
#' @param value An `immutable_list` object, being a class union between `list`
#'   and `immutable` S3 classes.
#'
#' @return Updates the object and returns invisibly.
#'
#' @keywords internal
setReplaceMethod("getIntern", signature(object="LongTable",
    value="immutable_list"), function(object, value) {
        object@.intern <- value
        return(object)
})

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
#'     the positional argument to use.names from the rowData generic. It
#'     doesn't do anything at this time and can be ignored.
#' @param ... For developer use only! Pass raw=TRUE to modify the slot
#'   directly. This will corrupt your data if you don't know what you are
#'   doing!
#'
#' @return A `data.table` containing rowID, row identifiers, and row metadata.
#'
#' @importFrom data.table data.table copy
#' @export
setMethod('rowData', signature(x='LongTable'),
        function(x, key=FALSE, use.names=FALSE, ...) {
    if (any(...names() == "raw") && isTRUE(...elt(which(...names() == "raw")))) {
        return(x@rowData)
    } else {
        return(if (key) copy(x@rowData[, -'.rownames']) else
            copy(x@rowData[, -c('.rownames', 'rowKey')]))
    }
})

#' Helper method to share functionality between rowData and colData replace methods
#'
#' @param x `LongTable` or inheriting class to update dimData for.
#' @param dim `character(1)` One of "row" or "col" indicating with dimension
#'   to updated metadata for.
#' @param value #' @param value A `data.table` or `data.frame` to update the
#'   `rowData` or `colData` of `x` with.
#'
#' @return An updated version of `value` which meets all the requirements for
#'   assignment to a `LongTable` or inheriting class.
#'
#' @noRd
#' @keywords internal
.update_dimData <- function(x, dim, value) {

    titleDim <- paste0(toupper(substr(dim, 1, 1)), substr(dim, 2, nchar(dim)))
    dimIDs <- get(paste0(dim, "IDs"))
    dimKey <- paste(dim, "Key")
    dimData <- paste0(dim, "Data")

    # type check input
    if (is(value, 'data.frame')) setDT(value)
    if (!is(value, 'data.table'))
        stop(.errorMsg('\n[CoreGx::', dim, 'Data<-] Please pass a data.frame or ',
            'data.table to update the ', dim, 'Data slot. We recommend modifying the',
            ' object returned by ', dim, 'Data(x) then reassigning it with ',
            dim, 'Data(x)',
            ' <- new', titleDim, 'Data'),
            call.=FALSE
        )

    # remove key column
    if (dimKey %in% colnames(value)) {
        value[, (dimKey) := NULL]
        .message('\n[CoreGx::', dim, ,'Data<-] Dropping ', dim, 'Key from replacement',
            ' value, this function will deal with mapping the ', dim, 'Key',
            ' automatically.')
    }

    # assemble information to select proper update method
    dimIDCols <- dimIDs(x)
    sharedDimIDCols <- intersect(dimIDCols, colnames(value))

    # error if all the rowID columns are not present in the new rowData
    equalDimIDs <- dimIDCols %in% sharedDimIDCols
    if (!all(equalDimIDs)) warning(.warnMsg('\n[CoreGx::', dim,
        'Data<-] The ID columns ', dimIDCols[!equalDimIDs],
        ' are not present in value. The function ',
        'will attempt to join with existing ', dim, 'IDs, but this may fail!',
        collapse=', '), call.=FALSE)

    dimIDs_ <- dimIDs(x, data=TRUE, key=TRUE)

    ## TODO:: Throw error if user tries to modify ID columns

    duplicatedIDcols <- value[, .N, by=c(sharedDimIDCols)][, N > 1]
    if (any(duplicatedIDcols))
        warning(.warnMsg("\n[CoreGx::", dim, "Data<-,", class(x)[1], "-method] The ",
            "ID columns are duplicated for rows ",
            .collapse(which(duplicatedIDcols)),
            "! These rows will be dropped before assignment."),
        call.=FALSE)

    dimData <- dimIDs_[unique(value), on=.NATURAL, allow.cartesian=FALSE]
    dimData[, (dimKey) := .I]
    dimData <- dimData[!duplicated(get(dimKey)), ]
    setkeyv(dimData, dimKey)
    dimData[, .rownames := Reduce(.paste_colon, mget(dimIDCols))]

    ## TODO:: Add some sanity checks before returing

    return(dimData)
}


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
#' @param value A `data.table` or `data.frame` to update the `rowData` of
#'   `x` with.
#' @param ... For developer use only! Pass raw=TRUE to modify the slot
#'   directly. This will corrupt your data if you don't know what you are
#'   doing!
#'
#' @return A copy of the `LongTable` object with the `rowData`
#'   slot updated.
#'
#' @md
#' @importFrom crayon cyan magenta
#' @importFrom SummarizedExperiment `rowData<-`
#' @importFrom data.table setDT
#' @export
setReplaceMethod('rowData', signature(x='LongTable'), function(x, ..., value) {

    if (any(...names() == "raw") && isTRUE(...elt(which(...names() == "raw")))) {
        x@rowData <- value
        return(invisible(x))
    }

    x@rowData <- .update_dimData(x=x, dim="row", value=value)
    return(invisible(x))
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
#' @param key `logical` Should the colKey column also be returned? Defaults to
#'     FALSE.
#' @param ... For developer use only! Pass raw=TRUE to return the slot for
#'   modification by reference.
#'
#' @return A `data.table` containing row identifiers and metadata.
#'
#' @import data.table
#' @export
setMethod('colData', signature(x='LongTable'),
        function(x, key=FALSE, dimnames=FALSE, ...) {
    if (any(...names() == "raw") && isTRUE(...elt(which(...names() == "raw")))) {
        return(x@colData)
    }
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
#' @param ... For developer use only! Pass raw=TRUE to modify the slot
#'   directly. This will corrupt your data if you don't know what you are
#'   doing!
#'
#' @return A copy of the `LongTable` object with the `colData`
#'   slot updated.
#'
#' @importFrom crayon cyan magenta
#' @importFrom SummarizedExperiment colData<-
#' @importFrom data.table data.table setDT
#' @export
setReplaceMethod('colData', signature(x='LongTable'),
        function(x, ..., value) {
    if (any(...names() == "raw") && isTRUE(...elt(which(...names() == "raw")))) {
        x@colData <- value
        return(invisible(x))
    }
    x@colData <- .update_dimData(x=x, dim="col", value=value)
    return(invisible(x))
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
#'   the row and column identifiers (i.e., the pseudo-dimnames of the object).
#' @param metadata `logical` Should row and column metadata also be joined
#'   to the returned assays. This is useful for modifying assays before
#'   reconstructing a new LongTable.
#' @param key `logical` Should the key columns also be returned? Defaults
#'   to !`withDimnames`.
#' @param ... For developer use only! Pass raw=TRUE to return the slot for
#'   modification by reference.
#'
#' @return A `list` of `data.table` objects, one per assay in the object.
#'
#' @importMethodsFrom SummarizedExperiment assays
#' @import data.table
#' @export
setMethod('assays', signature(x='LongTable'), function(x, withDimnames=TRUE,
        metadata=withDimnames, key=!withDimnames, ...) {
    # secret arguments for internal use
    if (any(...names() == "raw") && isTRUE(...elt(which(...names() == "raw")))) {
        return(x@assays)
    }

    # input validation
    if (!withDimnames && metadata)
        warning(.warnMsg('[CoreGx::assays] Cannot use metadata=TRUE when',
            ' withDimnames=FALSE. Ignoring the metadata argument.'),
            call.=FALSE)

    # optionally join with rowData and colData
    assayIndex <- assayIndex(x)
    if (metadata) {
        rData <- rowData(x, key=TRUE)
        cData <- colData(x, key=TRUE)
    } else {
        rData <- rowIDs(x, data=TRUE, key=TRUE)
        cData <- colIDs(x, data=TRUE, key=TRUE)
    }
    if (withDimnames) {
        setkeyv(assayIndex, "rowKey")
        assayIndex <- rData[assayIndex, , on="rowKey"]
        setkeyv(assayIndex, "colKey")
        assayIndex <- cData[assayIndex, , on="colKey"]
    }

    # honor row and column ordering guarantees from CoreGx design documentation
    aList <- copy(x@assays)
    aNames <- names(aList)
    # prepend with . to match assay index naming convention
    names(aList) <- paste0(".", aNames)
    corder <- c(
        if (withDimnames) idCols(x),
        if (key) c("rowKey", "colKey"),
        if (withDimnames && metadata) c(sort(rowMeta(x)), sort(colMeta(x)))
    )
    for (i in seq_along(aList)) {
        setkeyv(assayIndex, names(aList)[i])
        aList[[i]] <- assayIndex[aList[[i]], ]
        aList[[i]][, (setdiff(names(aList), names(aList)[i])) := NULL]
        if (withDimnames || key) aList[[i]][, (names(aList)[i]) := NULL]
        if (!key) {
            aList[[i]][, c("rowKey", "colKey") := NULL]
        }
        if (withDimnames) setkeyv(aList[[i]], idCols(x))
        else if (key) setkeyv(aList[[i]], c("rowKey", "colKey"))
        else setkeyv(aList[[i]], names(aList)[[i]])
        if (!is.null(corder)) setcolorder(aList[[i]], corder) else
            setcolorder(aList[[i]])
    }
    # reset names to no dot version
    names(aList) <- aNames
    return(aList)
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
#' @param ... For developer use only! Pass raw=TRUE to modify the slot
#'   directly. This will corrupt your data if you don't know what you are
#'   doing!
#'
#' @return A copy of the `LongTable` with the assays modified.
#'
#' @importMethodsFrom SummarizedExperiment assays<-
#' @import data.table
#' @export
setReplaceMethod('assays', signature(x='LongTable', value='list'),
        function(x, ..., value) {
    if (any(...names() == "raw") && isTRUE(...elt(which(...names() == "raw")))) {
        x@assays <- value
    } else {
        assay_names <- names(value)
        for (name in assay_names) {
            x[[name]] <- value[[name]]
        }
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
#' @param withDimnames `logical(1)` Should the dimension names be returned
#'   joined to the assay. This retrieves both the row and column identifiers
#'   and returns them joined to the assay. For
#' @param summarize `logical(1)` If the assays is a summary where some of
#'   `idCols(x)` are not in `assayKeys(x, i)`, then those missing columns
#'   are dropped. Defaults to `FALSE`. When `metadata` is `TRUE`, only
#'   metadata columns with 1:1 cardinality with the assay keys for `i`.
#' @param metadata `logical(1)` Should all of the metadata also be joined to
#'   the assay. This is useful when modifying assays as the resulting list
#'   has all the information needed to recreated the LongTable object. Defaults
#'   to `withDimnames`.
#' @param key `logical` Should the key columns also be returned? Defaults to
#'   !withDimnames. This is incompatible with `summarize=TRUE`, which will
#'   drop the key columns regardless of the value of this argument.
#'
#' @param ... For developer use only! Pass raw=TRUE to return the slot for
#'   modification by reference.
#'
#' @importMethodsFrom SummarizedExperiment assay
#' @importFrom crayon magenta cyan
#' @import data.table
#' @export
setMethod('assay', signature(x='LongTable'), function(x, i, withDimnames=TRUE,
        summarize=withDimnames, metadata=!summarize,
        key=!(summarize || withDimnames), ...) {
    # secret arguments for internal use
    if (any(...names() == "raw") && isTRUE(...elt(which(...names() == "raw")))) {
        return(x@assays[[i]])
    }

    ## TODO:: Update input validation to use checkmate where possible
    # validate input
    if (length(i) > 1)
        .error('\n[CoreGx::assay] Please specifying a single string ',
            'assay name or integer index. See assayNames(x) for available ',
            'assays.')

    keepAssay <- if (is.character(i)) which(assayNames(x) == i) else i
    assayName <- assayNames(x)[keepAssay]
    .assayName <- paste0(".", assayName)
    if (length(keepAssay) < 1)
        stop(.errorMsg('\n[CoreGx::assay] There is no assay ', i,
            ' in this LongTable. Use assayNames(longTable) for a list',
            'of valid assay names.'),
            call.=FALSE)

    if (!withDimnames && metadata)
        warning(.warnMsg('\n[CoreGx::assay] Cannot use metadata=TRUE when',
            ' withDimnames=FALSE. Ignoring the metadata argument.'),
            call.=FALSE)

    if (summarize && key)
        warning(.warnMsg('\n[CoreGx::assay] Cannot use key=TRUE when',
            ' summarize=TRUE. Ignoring the key argument.'),
            call.=FALSE)

    # extract the specified assay
    assayData <- copy(x@assays[[keepAssay]])

    # optionally join to rowData and colData
    assayIndex <- na.omit(unique(assayIndex(x)[,
        c("rowKey", "colKey", .assayName),
        with=FALSE
    ]))

    # handle summarized assays
    aKeys <- assayKeys(x, assayName)
    # only compute summaries for assays that are summarized actually summarized
    summarize <- summarize && !all(idCols(x) %in% aKeys)
    if (summarize) {
        assayIndex <- assayIndex[, first(.SD), by=.assayName]
    }

    setkeyv(assayIndex, .assayName)
    assayData <- assayData[assayIndex, on=.assayName]
    setkeyv(assayData, "rowKey")
    if (withDimnames && !metadata) {
        assayData <- rowIDs(x, data=TRUE, key=TRUE)[assayData, ]
        setkeyv(assayData, "colKey")
        assayData <- colIDs(x, data=TRUE, key=TRUE)[assayData, ]
    } else if (withDimnames && metadata) {
        assayData <- rowData(x, key=TRUE)[assayData, ]
        setkeyv(assayData, "colKey")
        assayData <- colData(x, key=TRUE)[assayData, ]
    }
    # honour row and column ordering guarantees
    ## See: https://github.com/bhklab/CoreGx/wiki/CoreGx-Design-Documentation
    if (withDimnames || key) {
        assayData[, (.assayName) := NULL]
        if (withDimnames) setkeyv(assayData, idCols(x)) else
            setkeyv(assayData, c("rowKey", "colKey"))
    } else {
        setkeyv(assayData, .assayName)
    }
    if (!key) assayData[, c("rowKey", "colKey") := NULL]
    corder <- c(
        if (withDimnames) idCols(x),
        if (key) c("rowKey", "colKey"),
        if (withDimnames && metadata) c(sort(rowMeta(x)), sort(colMeta(x)))
    )
    if (!is.null(corder)) setcolorder(assayData, corder) else setcolorder(assayData)

    # Drop columns with wrong cardinality from summary assays
    if (summarize) {
        summaryCols <- assayCols(x, assayName)
        if (metadata) {
            rCols <- colnames(rowData(x))
            cCols <- colnames(colData(x))
            rBy <- intersect(rCols, aKeys)
            cBy <- intersect(cCols, aKeys)
            rKeep <- rCols[
                rowData(x)[, lapply(.SD, uniqueN), by=c(rBy)][, lapply(.SD, max)] == 1
            ]
            cKeep <- cCols[
                colData(x)[, lapply(.SD, uniqueN), by=c(cBy)][, lapply(.SD, max)] == 1
            ]
            summaryCols <- c(summaryCols, rKeep, cKeep)
        }
        # use of %in% should maintain column ordering gaurantees
        assayData <- assayData[, colnames(assayData) %in% summaryCols, with=FALSE]
        setkeyv(assayData, aKeys)
    }

    ## Add [] to ensure assay always prints, even after modify by reference
    ## See: https://stackoverflow.com/questions/33195362/data-table-is-not-displayed-on-first-call-after-being-modified-in-a-function
    return(assayData[])
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
#'   with the dimnames.
#'
#' @return `LongTable` With updated assays slot.
#'
#' @describeIn LongTable
#'
#' @md
#' @importMethodsFrom SummarizedExperiment assay<-
#' @importFrom data.table data.table fsetdiff setcolorder set setDT
#' @export
setReplaceMethod('assay', signature(x='LongTable'), function(x, i, value) {
    stopifnot(is.character(i) || is.numeric(i))

    funContext <- CoreGx:::.S4MethodContext('assay', class(x))
    if (length(i) > 1) .error(funContext, ' Only a single assay ',
        'name can be assiged with assay(x, i) <- value.')

    # -- determine if the assay already exists
    if (is.numeric(i)) i <- assayNames(x)[i]
    .i <- paste0(".", i)
    assayExists <- i %in% assayNames(x)

    if (is.null(value)) {
        keepAssays <- setdiff(assayNames(x), i)
        return(x[, , keepAssays])
    }

    if (!is.data.frame(value)) .error(funContext, ' Only a data.frame or',
        ' data.table can be assiged to the assay slot!')
    value <- copy(value)  # prevent modify by reference
    if (!is.data.table(value)) setDT(value)

    # -- extract strucutral metadata form .intern slot
    mutable_intern <- mutable(getIntern(x))
    aIndex <- mutable_intern$assayIndex
    aKeys <- mutable_intern$assayKeys



    # -- determine the id columns if the assay doesn't already exits
    if (!any(assayExists)) {
        assayKey <- intersect(idCols(x), colnames(value))
    } else {
        if (sum(assayExists) > 1)
            .error(funContext, "Only one assay can be modified at a time.",
                " Please set i to be a character(1) vector.")
        assayKey <- aKeys[[i]]
    }
    # -- add assayKey column to the value
    setkeyv(value, assayKey)
    value[, (.i) := .I]

    # -- join assay with existing metadata
    rKeys <- intersect(rowIDs(x), assayKey)
    cKeys <- intersect(colIDs(x), assayKey)
    rIndex <- rowIDs(x, data=TRUE, key=TRUE)[, .SD, .SDcols=c("rowKey", rKeys)]
    setkeyv(rIndex, rKeys)
    cIndex <- colIDs(x, data=TRUE, key=TRUE)[, .SD, .SDcols=c("colKey", cKeys)]
    setkeyv(cIndex, cKeys)

    # -- add an index to the assay
    setkeyv(rIndex, "rowKey")
    setkeyv(cIndex, "colKey")
    setkeyv(aIndex, "rowKey")
    annotatedIndex <- merge.data.table(aIndex, rIndex, all=TRUE)
    setkeyv(annotatedIndex, "colKey")
    annotatedIndex <- merge.data.table(annotatedIndex, cIndex, all=TRUE)

    # -- update assayIndex with the new assay
    setkeyv(annotatedIndex, assayKey)
    if (.i %in% colnames(annotatedIndex)) annotatedIndex[, (.i) := NULL]
    # FIXME:: This is really slow with by=.EACHI when the cardinality is high
    annotatedIndex[value, (.i) := get(.i), on=assayKey, by=.EACHI]
    annotatedIndex[, (assayKey) := NULL]
    setkeyv(annotatedIndex, unique(c(paste0(".", assayNames(x)), .i)))

    # -- detect and warn users if they have modified id columns
    # rowIDs
    presentRowIDs <- intersect(rowIDs(x), colnames(value))  # allow summary over some keys
    if (!(length(presentRowIDs) > 0)) stop(.errorMsg("No rowIDs(x) present in",
        "value! Cannot summarize over an entire dimension."), call.=FALSE)
    ## set check.attributes=FALSE to allow unequal table keys
    equalRowIDs <- .table_is_subset(
        unique(value[, presentRowIDs, with=FALSE])[order(mget(presentRowIDs))],
        unique(rowIDs(x, data=TRUE)[order(mget(presentRowIDs)), presentRowIDs,
            with=FALSE])
    )
    if (!isTRUE(equalRowIDs))
        stop(.errorMsg("One or more rowIDs(x) columns have been modified.",
                " Identifier columns cannot be modified via assay assignment!"),
            call.=FALSE
        )
    # colIDs
    presentColIDs <- intersect(colIDs(x), colnames(value))  # allow summary over some keys
    if (!(length(presentColIDs) > 0)) stop(.errorMsg("No colIDs(x) present in",
        "value! Cannot summarize over an entire dimension."), call.=FALSE)
    equalColIDs <- .table_is_subset(
        unique(value[, presentColIDs, with=FALSE])[order(mget(presentColIDs))],
        unique(colIDs(x, data=TRUE)[order(mget(presentColIDs)), presentColIDs,
            with=FALSE])
    )
    if (!isTRUE(equalColIDs))
        stop(.errorMsg("One or more colIDs(x) column have been modified.",
                " Identifier columns cannot be modified via assay assignment!"),
            call.=FALSE
        )

    # -- remove metadata columns for the assay
    throwAwayCols <- c(idCols(x), rowMeta(x), colMeta(x))
    keepCols <- setdiff(colnames(value), throwAwayCols)
    assayValue <- unique(value[, keepCols, with=FALSE])
    setkeyv(assayValue, .i)

    # -- update the object
    setcolorder(annotatedIndex, c("rowKey", "colKey"))
    mutable_intern$assayIndex <- annotatedIndex
    mutable_intern$assayKeys[[i]] <- assayKey
    x@.intern <- immutable(mutable_intern)
    x@assays[[i]] <- assayValue

    return(x)
})

##
## == assayNames


#' Retrieve the assay names from a `LongTable` object.
#'
#' @examples
#' assayNames(merckLongTable)
#' names(merckLongTable)
#'
#' @describeIn LongTable Return the names of the assays contained in a
#'   `LongTable`
#'
#' @param x A `LongTable` object to retrieve the assay names from.
#'
#' @return `character` Names of the assays contained in the `LongTable`.
#'
#' @importMethodsFrom SummarizedExperiment assayNames
#' @aliases names,LongTable-method names
#' @export
setMethod('assayNames', signature(x='LongTable'), function(x) {
    return(names(x@assays))
})
#' @export
setMethod("names", signature(x="LongTable"), function(x) {
    return(assayNames(x))
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